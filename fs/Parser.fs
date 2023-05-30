module Morth.Parser

let parseWord =
  function
  | "+" -> Op.Plus
  | "-" -> Op.Minus
  | "." -> Op.Dump
  | n -> Op.Push(int n)

let parse (input : string) =
  input.Split() |> Seq.filter (fun x -> x.Length > 0) |> Seq.map parseWord
