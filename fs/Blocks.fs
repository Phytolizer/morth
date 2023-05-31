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
      | Op.Else _ ->
        let ifIp = stack.Pop() in

        match program.[ifIp] with
        | Op.If _ -> program.[ifIp] <- Op.If(ip + 1)
        | _ -> failwith "unmatched else"

        stack.Push ip
        loop (ip + 1)
      | Op.End ->
        let blockIp = stack.Pop() in

        match program.[blockIp] with
        | Op.If _ -> program.[blockIp] <- Op.If ip
        | Op.Else _ -> program.[blockIp] <- Op.Else ip
        | _ -> failwith "unmatched end"

        loop (ip + 1)
      | _ -> loop (ip + 1) in

  loop 0
  program |> Seq.ofArray
