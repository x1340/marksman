module Marksman.Diag

open System
open System.IO
open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Folder
open Marksman.Workspace
open Marksman.Syms

module Lsp = Ionide.LanguageServerProtocol.Types

open Marksman.Cst
open Marksman.Index
open Marksman.Refs

type Entry =
    | AmbiguousLink of Element * Syms.Ref * array<Dest>
    | BrokenLink of Element * Syms.Ref
    | NonBreakableWhitespace of Lsp.Range

let code: Entry -> string =
    function
    | AmbiguousLink _ -> "1"
    | BrokenLink _ -> "2"
    | NonBreakableWhitespace _ -> "3"

let checkNonBreakingWhitespace (doc: Doc) =
    let nonBreakingWhitespace = "\u00a0"

    let headings = [ 1..7 ] |> List.map (fun n -> (String.replicate n "#"))

    [ 0 .. (Doc.text doc).lineMap.NumLines ]
    |> List.collect (fun x ->
        let line = (Doc.text doc).LineContent x

        let headingLike =
            List.tryFind (fun (h: string) -> line.StartsWith(h + nonBreakingWhitespace)) headings

        match headingLike with
        | None -> []
        | Some heading ->
            let whitespaceRange: Lsp.Range = {
                Start = { Line = x; Character = heading.Length }
                End = { Line = x; Character = heading.Length + 1 }
            }

            [ NonBreakableWhitespace(whitespaceRange) ])

// Check if a link target exists on file system (for non-markdown files/directories)
let tryResolveNonMarkdownPath (folder: Folder) (doc: Doc) (linkPath: string) : bool =
    try
        let decodedPath = linkPath.UrlDecode()

        // Handle file:// URIs
        let pathStr =
            if decodedPath.StartsWith("file://", StringComparison.OrdinalIgnoreCase) then
                Uri(decodedPath).LocalPath
            else
                decodedPath

        // Resolve path
        let absPath =
            if pathStr.StartsWith('/') || pathStr.StartsWith('\\') then
                // Check if it's already an absolute system path
                if Path.IsPathRooted(pathStr) then
                    // Use as is
                    pathStr
                else
                    // Combine with current file path.
                    let currentPath = (Folder.id folder).data |> RootPath.toSystem
                    Path.Combine(currentPath, pathStr.TrimStart('/', '\\'))
            else
                // Relative from document directory
                let docDir = Doc.path doc |> AbsPath.toSystem |> Path.GetDirectoryName
                Path.Combine(docDir, pathStr)

        let normalizedPath = Path.GetFullPath(absPath)

        // Check existence
        File.Exists(normalizedPath) || Directory.Exists(normalizedPath)
    with
    | _ -> false

// Check if a path is an external URL (consider file:// as internal URL)
let isExternalUrl (path: string) : bool =
    Uri.IsWellFormedUriString(path, UriKind.Absolute) &&
    not (path.StartsWith("file://", StringComparison.OrdinalIgnoreCase))

// Check a non-markdown target and return diagnostic if it doesn't exist
let checkNonMarkdownTarget (folder: Folder) (doc: Doc) (linkEl: Element) (targetPath: string) (refOpt: option<Ref>) : list<Entry> =
    if tryResolveNonMarkdownPath folder doc targetPath then
        [] // Exists, no diagnostic
    else
        // Missing - create diagnostic
        let refToUse =
            match refOpt with
            | Some r -> r
            | None -> Ref.CrossRef(CrossRef.CrossDoc targetPath)
        [ BrokenLink(linkEl, refToUse) ]

let checkLink (folder: Folder) (doc: Doc) (linkEl: Element) : seq<Entry> =
    let exts = Folder.configuredMarkdownExts folder

    let ref =
        doc.Structure
        |> Structure.Structure.tryFindSymbolForConcrete linkEl
        |> Option.bind Syms.Sym.asRef

    match ref with
    | None ->
        match linkEl with
        | ML { data = MdLink.IL(_, url, _) } ->
            match url with
            | Some { data = url } ->
                let decodedUrl = UrlEncoded.decode url
                if isExternalUrl decodedUrl then
                    [] // External URL - no diagnostic
                else if Misc.isPotentiallyInternalRef exts decodedUrl then
                    [] // Markdown file - already handled by symbol system
                else
                    // Non-markdown file/directory - check existence
                    checkNonMarkdownTarget folder doc linkEl decodedUrl None
            | None -> []
        | _ -> []
    | Some ref ->
        let refs = Dest.tryResolveElement folder doc linkEl |> Array.ofSeq

        if Folder.isSingleFile folder && Syms.Ref.isCross ref then
            []
        else if refs.Length = 1 then
            []
        else if refs.Length = 0 then
            match linkEl with
            // Inline shortcut links often are a part of regular text.
            // Raising diagnostics on them would be noisy.
            | ML { data = MdLink.RS _ } -> []
            | ML { data = MdLink.IL(_, url, _) } ->
                match url with
                | Some { data = url } ->
                    let decodedUrl = UrlEncoded.decode url
                    if Misc.isMarkdownFile exts decodedUrl then
                        [ BrokenLink(linkEl, ref) ] // markdown
                    else
                        checkNonMarkdownTarget folder doc linkEl decodedUrl (Some ref)
                | _ -> [ BrokenLink(linkEl, ref) ] // non-markdown
            | WL { data = wl } ->
                match wl.doc with
                | Some docNode ->
                    let docPath = WikiEncoded.decode docNode.data
                    if isExternalUrl docPath then
                        [] // External URL - no diagnostic
                    else if Misc.isMarkdownFile exts docPath then
                        [ BrokenLink(linkEl, ref) ] // markdown - diagnostic
                    else
                        // Non-markdown file/directory - check existence
                        checkNonMarkdownTarget folder doc linkEl docPath (Some ref)
                | None -> [ BrokenLink(linkEl, ref) ]
            | _ -> [ BrokenLink(linkEl, ref) ]
        else
            [ AmbiguousLink(linkEl, ref, refs) ]

let checkLinks (folder: Folder) (doc: Doc) : seq<Entry> =
    let links = Doc.index >> Index.links <| doc
    links |> Seq.collect (checkLink folder doc)

let checkFolder (folder: Folder) : seq<DocId * list<Entry>> =
    seq {
        for doc in Folder.docs folder do
            let docDiag =
                seq {
                    yield! checkLinks folder doc
                    yield! checkNonBreakingWhitespace doc
                }
                |> List.ofSeq

            Doc.id doc, docDiag
    }

let destToHuman (ref: Dest) : string =
    match ref with
    | Dest.Doc { doc = doc } -> $"document {Doc.name doc}"
    | Dest.Heading(docLink, { data = heading }) ->
        $"heading {Heading.name heading} in the document {Doc.name (DocLink.doc docLink)}"
    | Dest.LinkDef(_, { data = ld }) -> $"link definition {MdLinkDef.name ld}"
    | Dest.Tag(doc, { data = tag }) -> $"tag {tag.name} in the document {Doc.name doc}"

let docToHuman (name: string) : string = $"document '{name}'"

let refToHuman (ref: Syms.Ref) : string =
    match ref with
    | Syms.CrossRef(Syms.CrossDoc docName) -> docToHuman docName
    | Syms.CrossRef(Syms.CrossSection(docName, sectionName)) ->
        $"heading '{Slug.toString sectionName}' in {docToHuman docName}"
    | Syms.IntraRef(Syms.IntraSection heading) -> $"heading '{Slug.toString heading}'"
    | Syms.IntraRef(Syms.IntraLinkDef ld) -> $"link definition with the label '{ld}'"

let diagToLsp (diag: Entry) : Lsp.Diagnostic =
    match diag with
    | AmbiguousLink(el, ref, dests) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | T _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let mkRelated dest : DiagnosticRelatedInformation =
            let loc = Dest.location dest
            let msg = $"Duplicate definition of {refToHuman ref}"
            { Location = loc; Message = msg }

        let related = dests |> Array.map mkRelated

        {
            Range = Element.range el
            Severity = Some severity
            Code = Some(code diag)
            CodeDescription = None
            Source = Some "Marksman"
            Message = $"Ambiguous link to {refToHuman ref}"
            RelatedInformation = Some related
            Tags = None
            Data = None
        }
    | BrokenLink(el, ref) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | T _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let msg = $"Link to non-existent {refToHuman ref}"

        {
            Range = Element.range el
            Severity = Some severity
            Code = Some(code diag)
            CodeDescription = None
            Source = Some "Marksman"
            Message = msg
            RelatedInformation = None
            Tags = None
            Data = None
        }

    | NonBreakableWhitespace dup -> {
        Range = dup
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "Marksman"
        Message =
            "Non-breaking whitespace used instead of regular whitespace. This line won't be interpreted as a heading"
        RelatedInformation = None
        Tags = None
        Data = None
      }

type FolderDiag = array<DocId * array<Lsp.Diagnostic>>

module FolderDiag =
    let mk (folder: Folder) : FolderDiag =
        checkFolder folder
        |> Seq.map (fun (uri, diags) ->
            let lspDiags = List.map diagToLsp diags |> Array.ofList

            uri, lspDiags)
        |> Array.ofSeq

type WorkspaceDiag = Map<FolderId, FolderDiag>

module WorkspaceDiag =
    let mk (ws: Workspace) : WorkspaceDiag =
        Workspace.folders ws
        |> Seq.map (fun folder -> (Folder.id folder), FolderDiag.mk folder)
        |> Map.ofSeq

    let empty = Map.empty
