module Morth.Main

open Morth.Logger
open System.IO
open System.Diagnostics
open FSharpx.Collections

[<Struct>]
type command_result =
  {
    exit_code : int
    stdout : string
    stderr : string
  }

let runCmd (command : string) (args : string array) =
  let args =
    args
    |> Array.map (
      (fun arg -> arg.Replace("\"", @"\""")) >> (fun arg -> "\"" + arg + "\"")
    )
    |> String.concat " " in

  cmd $@" ""{command}"" {args}"

  let info = new ProcessStartInfo(command, args) in
  info.RedirectStandardError <- true
  info.RedirectStandardOutput <- true
  info.UseShellExecute <- false
  let p = new Process() in
  p.StartInfo <- info
  p.Start() |> ignore
  p.WaitForExit()

  {
    exit_code = p.ExitCode
    stdout = p.StandardOutput.ReadToEnd()
    stderr = p.StandardError.ReadToEnd()
  }

let check =
  function
  | {
      exit_code = 0
      stdout = _
      stderr = _
    } -> ()
  | {
      exit_code = _
      stdout = _
      stderr = err
    } -> failwith <| sprintf "Command failed with stderr:\n%s" err

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
  let args = args |> Array.toSeq in

  match Seq.unCons args with
  | Some("help", _) -> usage ()
  | Some("sim", args) ->
    let file =
      (match Seq.unCons args with
       | Some(file, _) -> file
       | None ->
         usage ()
         error "expected arg to 'sim'"
         exit 1) in

    File.ReadAllText file |> Parser.parse |> Sim.simulate
  | Some("com", args) ->
    let file =
      (match Seq.unCons args with
       | Some(file, _) -> file
       | None ->
         usage ()
         error "expected arg to 'com'"
         exit 1) in

    let asmFile = Path.ChangeExtension(file, ".asm") in

    info "Generating %s" asmFile

    File.WriteAllText(
      asmFile,
      File.ReadAllText file |> Parser.parse |> Com.compile
    )

    let objFile = Path.ChangeExtension(file, ".o") in

    runCmd
      "nasm"
      [|
        "-f"
        "elf64"
        asmFile
        "-o"
        objFile
      |]
    |> check

    let exeFile = Path.ChangeExtension(file, null) in

    runCmd
      "ld"
      [|
        objFile
        "-o"
        exeFile
      |]
    |> check
  | _ ->
    usage ()
    exit 1

  0
