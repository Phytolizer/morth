module Morth.Blocks

open System.Collections.Generic
open Logger

let resolve (program : Op.t seq) =
  let program = Array.ofSeq program in
  let stack = new Stack<int>() in

  let rec loop ip =
    if ip >= program.Length then
      ()
    else
      let op = program.[ip] in

      match op.code with
      | Op.If _ ->
        stack.Push ip
        loop (ip + 1)
      | Op.Else _ ->
        let ifIp = stack.Pop() in

        match program.[ifIp].code with
        | Op.If _ -> program.[ifIp].code <- Op.If(ip + 1)
        | _ ->
          locError op.loc "This 'else' is unmatched."
          exit 1

        stack.Push ip
        loop (ip + 1)
      | Op.End _ ->
        let blockIp = stack.Pop() in

        match program.[blockIp].code with
        | Op.If _ ->
          program.[blockIp].code <- Op.If ip
          program.[ip].code <- Op.End(ip + 1)
        | Op.Else _ ->
          program.[blockIp].code <- Op.Else ip
          program.[ip].code <- Op.End(ip + 1)
        | Op.Do dest ->
          program.[ip].code <- Op.End dest
          program.[blockIp].code <- Op.Do(ip + 1)
        | _ ->
          locError op.loc "This 'end' is unmatched."
          exit 1

        loop (ip + 1)
      | Op.While ->
        stack.Push ip
        loop (ip + 1)
      | Op.Do _ ->
        let whileIp = stack.Pop() in
        program.[ip].code <- Op.Do whileIp
        stack.Push ip
        loop (ip + 1)
      | _ -> loop (ip + 1) in

  loop 0

  if stack.Count > 0 then
    let op = program.[stack.Pop()] in
    locError op.loc "This block is unclosed."
    exit 1

  program |> Seq.ofArray
