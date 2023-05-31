module Morth.Language.Sim

open System.Collections.Generic
open FSharpx.Collections
open System.IO

let bop (stack : int Stack) op =
  let b = stack.Pop() in
  let a = stack.Pop() in
  stack.Push(op a b)

let boolop (stack : int Stack) op =
  bop stack (fun a b -> if op a b then 1 else 0)

let handle_op (out : TextWriter) (stack : int Stack) ip (op : Op.t) =
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
    fprintfn out "%d" top
    ip + 1
  | Op.If dest -> let top = stack.Pop() in if top = 0 then dest else ip + 1
  | Op.Else dest -> dest
  | Op.While -> ip + 1
  | Op.Do dest -> let top = stack.Pop() in if top = 0 then dest else ip + 1
  | Op.End dest -> dest
  | Op.Mem -> failwith "unimplemented"

let simulate out ops =
  let program = ops |> Seq.toArray in

  let rec loop ip stack =
    if ip >= program.Length then
      ()
    else
      let op = program.[ip] in
      let ip = handle_op out stack ip op in
      loop ip stack in

  loop 0 (new Stack<int>())
