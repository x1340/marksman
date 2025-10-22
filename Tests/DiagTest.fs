module Marksman.DiagTest

open Xunit

open Marksman.Diag
open Marksman.Helpers
open Marksman.Index
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Folder
open Marksman.Config
open Marksman.Text

let entryToHuman (entry: Entry) =
    let lsp = diagToLsp entry
    lsp.Message

let diagToHuman (diag: seq<DocId * list<Entry>>) : list<string * string> =
    seq {
        for id, entries in diag do
            for e in entries do
                yield id.Path |> RootedRelPath.relPathForced |> RelPath.toSystem, entryToHuman e
    }
    |> List.ofSeq

[<Fact>]
let documentIndex_1 () =
    let doc = FakeDoc.Mk "# T1\n# T2"

    let titles =
        Doc.index >> Index.titles <| doc
        |> Array.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)

[<Fact>]
let nonBreakingWhitespace () =
    let nbsp = "\u00a0"
    let doc = FakeDoc.Mk $"# T1\n##{nbsp}T2"

    match (checkNonBreakingWhitespace doc) with
    | [ NonBreakableWhitespace range ] ->
        Assert.Equal(1, range.Start.Line)
        Assert.Equal(1, range.End.Line)

        Assert.Equal(2, range.Start.Character)
        Assert.Equal(3, range.End.Character)
    | _ -> failwith "Expected NonBreakingWhitespace diagnostic"

[<Fact>]
let noDiagOnShortcutLinks () =
    let doc = FakeDoc.Mk([| "# H1"; "## H2"; "[shortcut]"; "[[#h42]]" |])
    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent heading 'h42'" ], diag)

[<Fact>]
let noDiagOnRealUrls () =
    let doc =
        FakeDoc.Mk([| "# H1"; "## H2"; "[](www.bad.md)"; "[](https://www.good.md)" |])

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'www.bad.md'" ], diag)

[<Fact>]
let noDiagOnNonMarkdownFiles () =
    let doc =
        FakeDoc.Mk(
            [|
                "# H1"
                "## H2"
                "[](bad.md)"
                "[](another%20bad.md)"
            |]
        )

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>(
        [
            "fake.md", "Link to non-existent document 'bad.md'"
            "fake.md", "Link to non-existent document 'another bad.md'"
        ],
        diag
    )


[<Fact>]
let crossFileDiagOnBrokenWikiLinks () =
    let doc = FakeDoc.Mk([| "[[bad]]" |])

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'bad'" ], diag)

[<Fact>]
let noCrossFileDiagOnSingleFileFolders () =
    let doc =
        FakeDoc.Mk(
            [|
                "[](bad.md)" //
                "[[another-bad]]"
                "[bad-ref][bad-ref]"
            |]
        )

    let folder = Folder.singleFile doc None
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>(
        [
            "fake.md", "Link to non-existent link definition with the label 'bad-ref'"
        ],
        diag
    )

[<Fact>]
let issue_429 () =
    let testDir = "/tmp/marksman-test"
    let testFile = "test.md"

    // Create test directory and files
    System.IO.Directory.CreateDirectory(testDir) |> ignore
    System.IO.Directory.CreateDirectory(System.IO.Path.Combine(testDir, "subfolder")) |> ignore

    // Create test files that should exist
    System.IO.File.WriteAllText(System.IO.Path.Combine(testDir, "test.pdf"), "dummy pdf content")
    System.IO.File.WriteAllText(System.IO.Path.Combine(testDir, "image.png"), "dummy image content")
    System.IO.File.WriteAllText(System.IO.Path.Combine(testDir, "subfolder", "file.txt"), "dummy text content")

    // Create markdown document with links
    let absTestDir = System.IO.Path.GetFullPath(testDir)
    let content = $"""# Test Document

## Links to existing files (relative paths)
- [Link to PDF](test.pdf)
- [Link to image](image.png)
- [Link to subfolder file](subfolder/file.txt)
- [[test.pdf]]
- [[image.png]]
- [[subfolder/file.txt]]

## Links to existing files (absolute paths)
- [Absolute PDF]({absTestDir}/test.pdf)
- [Absolute image]({absTestDir}/image.png)
- [[{absTestDir}/test.pdf]]
- [[{absTestDir}/image.png]]
- [[{absTestDir}/subfolder/file.txt]]

## Links to missing files (relative paths)
- [Missing PDF](missing.pdf)
- [Missing image](missing.png)
- [[missing.pdf]]
- [[missing-file]]

## Links to missing files (absolute paths)
- [Absolute missing]({absTestDir}/missing.pdf)
- [[{absTestDir}/missing.pdf]]
- [[{absTestDir}/missing-file]]

## External links
- [External](https://example.com)
- [External with file](https://example.com/file.pdf)
- [HTTP](http://example.com/file.pdf)
- [HTTPS](https://example.com/doc.pdf)
- [FTP](ftp://example.com/file.txt)
- [Mailto](mailto:test@example.com)
- [Data URI](data:text/plain;base64,SGVsbG8=)
- [[https://example.com/file.pdf]]
- [[http://example.com]]
- [[ftp://ftp.example.com/file.zip]]

## file:// URIs (internal - should be checked)
- [File URI existing](file://{absTestDir}/test.pdf)
- [File URI missing](file://{absTestDir}/nonexistent.pdf)
"""

    let testPath = System.IO.Path.Combine(testDir, testFile)
    System.IO.File.WriteAllText(testPath, content)

    // Create files with absolute paths
    let absTestPath = System.IO.Path.GetFullPath(testPath)
    let rootUri = $"file://{absTestDir}"
    let folderId = UriWith.mkRoot rootUri
    let docUri = $"file://{absTestPath}"
    let docId = DocId(UriWith.mkRooted folderId (LocalPath.ofUri docUri))

    let text = Text.mkText content
    let doc = Doc.mk (ParserSettings.OfConfig(Config.Default)) docId None text

    let folder = Folder.multiFile "test-folder" folderId [doc] None
    let diag = checkFolder folder |> diagToHuman

    // No diagnostic for external URLs (http, https, ftp, mailto, data)
    let externalUrlErrors = diag |> List.filter (fun (_, msg) ->
        msg.Contains("example.com") ||
        msg.Contains("test@example.com") ||
        msg.Contains("base64"))

    let externalErrorMsg =
        if externalUrlErrors.IsEmpty then
            ""
        else
            let errors = externalUrlErrors |> List.map (fun (f, m) -> $"{f}: {m}") |> String.concat "\n  "
            $"Should not have diagnostics for external URLs, but found:\n  {errors}"

    Assert.True(externalUrlErrors.IsEmpty, externalErrorMsg)

    // Diagnostics for missing files (including file:// URI to missing file)
    let missingOnly = diag |> List.filter (fun (_, msg) ->
        msg.Contains("missing.pdf") ||
        msg.Contains("missing.png") ||
        msg.Contains("missing-file") ||
        msg.Contains("nonexistent.pdf"))

    // Should have diagnostics for missing files
    Assert.NotEmpty(missingOnly)

    // Should NOT have diagnostics for existing files (test.pdf, image.png, subfolder/file.txt)
    let existingFileErrors = diag |> List.filter (fun (_, msg) ->
        (msg.Contains("test.pdf") ||
         msg.Contains("image.png") ||
         msg.Contains("subfolder/file.txt")) &&
        not (msg.Contains("missing")))

    let errorMsg =
        if existingFileErrors.IsEmpty then
            "Should not have diagnostics for existing files"
        else
            let errors = existingFileErrors |> List.map (fun (f, m) -> $"{f}: {m}") |> String.concat "\n  "
            $"Should not have diagnostics for existing files, but found:\n  {errors}"

    Assert.True(existingFileErrors.IsEmpty, errorMsg)

    // Clean up test directory
    try
        System.IO.Directory.Delete(testDir, true)
    with
    | _ -> () // Ignore cleanup errors
