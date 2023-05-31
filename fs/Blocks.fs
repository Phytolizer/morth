module Morth.Blocks

open System.Collections.Generic

let resolve program =
  let program = Array.ofSeq program in
  let stack = new Stack<int>() in

  let rec loop ip =
    if ip >= program.Length then
      ()
    else
      let op = program.[ip] in

      match op with
      | Op.If _ ->
        stack.Push ip
        loop (ip + 1)
      | Op.End ->
        let dest = stack.Pop() in

        match program.[dest] with
        | Op.If _ -> program.[dest] <- Op.If ip
        | _ -> failwith "unmatched end"

        loop (ip + 1)
      | _ -> loop (ip + 1) in

  loop 0
  program |> Seq.ofArray
