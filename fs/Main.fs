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

[<EntryPoint>]
let main (args : string array) =
  let program =
    [|
      Op.Push 34
      Op.Push 35
      Op.Plus
      Op.Dump
      Op.Push 500
      Op.Push 80
      Op.Minus
      Op.Dump
    |] in

  File.WriteAllText("output.asm", Com.compile program)

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

  0
