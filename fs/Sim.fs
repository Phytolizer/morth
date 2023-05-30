module Morth.Sim

open System.Collections.Generic

let bop (stack : int Stack) op =
  let b = stack.Pop() in
  let a = stack.Pop() in
  stack.Push(op a b)

let handle_op (stack : int Stack) =
  function
  | Op.Push n -> stack.Push n
  | Op.Plus -> bop stack (+)
  | Op.Minus -> bop stack (-)
  | Op.Dump -> let top = stack.Pop() in printfn "%d" top

let simulate (ops : Op.t seq) = Seq.iter (handle_op (Stack<int>())) ops
