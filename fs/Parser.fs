module Morth.Parser

let parseWord (token : Token.t) =
  match token.word with
  | "+" -> Op.Plus
  | "-" -> Op.Minus
  | "." -> Op.Dump
  | n ->
    try
      Op.Push(int n)
    with err ->
      Printf.eprintfn "%s: %s" (token.loc.ToString()) err.Message
      exit 1

let parse (filePath : string) =
  Lexer.lexFile filePath |> Seq.map parseWord
