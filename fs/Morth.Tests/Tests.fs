module Tests

open Swensen.Unquote.Assertions
open Xunit
open System.IO
open Morth.Language

let loadTests () =
  let dir = __SOURCE_DIRECTORY__ in
  let dir = Path.Join(dir, "..", "tests") in
  let dir = Path.GetFullPath(dir) in

  Directory.GetFiles(dir, "*.porth")
  |> Array.sort
  |> Array.map (fun x -> [| x :> obj |])

[<Theory>]
[<MemberData(nameof (loadTests))>]
let ``Run morth`` (path : string) =
  let simOutput = new StringWriter() in

  let simResult =
    (Driver.run
      [|
        "sim"
        path
      |]
      simOutput) in

  test <@ simResult = 0 @>
  let comOutput = new StringWriter() in

  let comResult =
    (Driver.run
      [|
        "com"
        "-r"
        path
      |]
      comOutput) in

  test <@ comResult = 0 @>
  test <@ simOutput.ToString() = comOutput.ToString() @>
