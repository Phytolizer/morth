module Morth.Sim

open System.Collections.Generic

let bop (stack : int Stack) op =
  let b = stack.Pop() in
  let a = stack.Pop() in
  stack.Push(op a b)

let boolop (stack : int Stack) op =
  bop stack (fun a b -> if op a b then 1 else 0)

let handle_op (stack : int Stack) =
  function
  | Op.Push n -> stack.Push n
  | Op.Plus -> bop stack (+)
  | Op.Minus -> bop stack (-)
  | Op.Eq -> boolop stack (=)
  | Op.Dump -> let top = stack.Pop() in printfn "%d" top

let simulate (ops : Op.t seq) = Seq.iter (handle_op (Stack<int>())) ops
