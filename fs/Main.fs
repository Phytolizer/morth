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

  cmd $@"""{command}"" {args}"

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

let showOutput
  {
    exit_code = _
    stdout = out
    stderr = err
  }
  =
  eprintf "%s" err
  printf "%s" out

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
      "  sim <FILE>             Simulate a morth program"
      "  com <FILE>             Compile a morth program"
      "    OPTIONS:"
      "      -r                 Run the compiled program"
      "      -o <FILE|DIR>      Output file or directory"
    |]

let resolveOutPath (outPath : string) (inFile : string) =
  if outPath = null then
    inFile
  else
    try
      let attrs = File.GetAttributes(outPath) in

      if attrs.HasFlag(FileAttributes.Directory) then
        Path.Combine(outPath, Path.GetFileNameWithoutExtension(inFile))
      else
        outPath
    with _ ->
      outPath

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

    Parser.parse file |> Sim.simulate
  | Some("com", args) ->
    let (run, outPath, file) =
      (let rec loop run outPath args =
        (match Seq.unCons args with
         | Some("-r", args) -> loop true outPath args
         | Some("-o", args) ->
           (match Seq.unCons args with
            | Some(outPath, args) -> loop run outPath args
            | None ->
              usage ()
              error "expected arg to '-o'"
              exit 1)
         | Some(file, _) -> run, outPath, file
         | None ->
           usage ()
           error "expected arg to 'com'"
           exit 1) in

       loop false null args)

    let outPath = resolveOutPath outPath file in
    let asmFile = Path.ChangeExtension(outPath, ".asm") in

    info "Generating %s" asmFile

    File.WriteAllText(asmFile, Parser.parse file |> Com.compile)

    let objFile = Path.ChangeExtension(outPath, ".o") in

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

    let exeFile = Path.ChangeExtension(outPath, null) in

    runCmd
      "ld"
      [|
        objFile
        "-o"
        exeFile
      |]
    |> check

    if run then
      runCmd
        (if Path.IsPathRooted exeFile then
           exeFile
         else
           "./" + exeFile)
        [||]
      |> showOutput
  | _ ->
    usage ()
    exit 1

  0
