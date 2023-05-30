module Morth.Main

open System.IO
open System.Diagnostics

let runCmd (cmd : string) (args : string array) =
  let args =
    args
    |> Array.map (
      (fun arg -> arg.Replace("\"", "\\\"")) >> (fun arg -> "\"" + arg + "\"")
    )
    |> String.concat " " in

  let info = new ProcessStartInfo(cmd, args) in
  info.RedirectStandardError <- true
  info.RedirectStandardOutput <- true
  info.UseShellExecute <- false
  let p = new Process() in
  p.StartInfo <- info
  p.Start() |> ignore
  p.WaitForExit()
  p.ExitCode, p.StandardOutput.ReadToEnd(), p.StandardError.ReadToEnd()

let check =
  function
  | (0, _, _) -> ()
  | (_, _, err) -> failwith <| sprintf "Command failed with stderr: %s" err

let usage () =
  Array.iter
    printfn
    [|
      "Usage: morth <SUBCOMMAND> [ARGS]"
      "SUBCOMMANDS:"
      "  sim <FILE>    Simulate a morth program"
      "  com <FILE>    Compile a morth program"
    |]

[<EntryPoint>]
let main (args : string array) =
  match args with
  | [| "sim"; file |] -> File.ReadAllText file |> Parser.parse |> Sim.simulate
  | [| "com"; file |] ->
    File.WriteAllText(
      "output.asm",
      File.ReadAllText file |> Parser.parse |> Com.compile
    )

    runCmd
      "nasm"
      [|
        "-felf64"
        "output.asm"
      |]
    |> check

    runCmd
      "ld"
      [|
        "-o"
        "output"
        "output.o"
      |]
    |> check
  | _ ->
    usage ()
    exit 1

  0
