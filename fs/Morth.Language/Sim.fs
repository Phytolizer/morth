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

let handle_op
  (out : TextWriter)
  (stack : int Stack)
  (mem : byte array)
  ip
  (op : Op.t)
  =
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
  | Op.Mem ->
    stack.Push 0
    ip + 1
  | Op.Load ->
    let addr = stack.Pop() in
    let b = mem.[addr] in
    stack.Push(int b)
    ip + 1
  | Op.Store ->
    let value = stack.Pop() in
    let addr = stack.Pop() in
    mem.[addr] <- byte value
    ip + 1
  | Op.Syscall1
  | Op.Syscall2
  | Op.Syscall4
  | Op.Syscall5
  | Op.Syscall6 -> failwith "unimplemented"
  | Op.Syscall3 ->
    let syscall = stack.Pop() in
    let arg1 = stack.Pop() in
    let arg2 = stack.Pop() in
    let arg3 = stack.Pop() in

    (match syscall with
     | 1 ->
       let fd, buf, count = arg1, arg2, arg3 in
       let s = Array.sub mem buf count |> System.Text.Encoding.UTF8.GetString in

       (match fd with
        | 1 -> fprintf out "%s" s
        | 2 -> eprintf "%s" s
        | _ -> failwithf "unknown fd: %d" fd)
     | _ -> failwithf "unknown syscall %d" syscall)

    ip + 1

let simulate out ops =
  let program = ops |> Seq.toArray in
  let mem = Array.create Config.MEM_CAPACITY (byte 0) in

  let rec loop ip stack =
    if ip >= program.Length then
      ()
    else
      let op = program.[ip] in
      let ip = handle_op out stack mem ip op in
      loop ip stack in

  loop 0 (new Stack<int>())
