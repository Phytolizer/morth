module Morth.Sim

open System.Collections.Generic
open FSharpx.Collections

let bop (stack : int Stack) op =
  let b = stack.Pop() in
  let a = stack.Pop() in
  stack.Push(op a b)

let boolop (stack : int Stack) op =
  bop stack (fun a b -> if op a b then 1 else 0)

let handle_op (stack : int Stack) ip (op : Op.t) =
  match op.code with
  | Op.Push n ->
    stack.Push n
    ip + 1
  | Op.Dup ->
    let top = stack.Peek() in
    stack.Push top
    ip + 1
  | Op.Plus ->
    bop stack (+)
    ip + 1
  | Op.Minus ->
    bop stack (-)
    ip + 1
  | Op.Eq ->
    boolop stack (=)
    ip + 1
  | Op.Gt ->
    boolop stack (>)
    ip + 1
  | Op.Dump ->
    let top = stack.Pop() in
    printfn "%d" top
    ip + 1
  | Op.If dest -> let top = stack.Pop() in if top = 0 then dest else ip + 1
  | Op.Else dest -> dest
  | Op.While -> ip + 1
  | Op.Do dest -> let top = stack.Pop() in if top = 0 then dest else ip + 1
  | Op.End dest -> dest

let simulate ops =
  let program = ops |> Seq.toArray in

  let rec loop ip stack =
    if ip >= program.Length then
      ()
    else
      let op = program.[ip] in
      let ip = handle_op stack ip op in
      loop ip stack in

  loop 0 (new Stack<int>())
