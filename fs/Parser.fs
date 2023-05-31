module Morth.Parser

let parseWord (token : Token.t) =
  match token.word with
  | "+" -> Op.Plus
  | "-" -> Op.Minus
  | "=" -> Op.Eq
  | "." -> Op.Dump
  | "if" -> Op.If 0
  | "else" -> Op.Else 0
  | "end" -> Op.End
  | n ->
    try
      Op.Push(int n)
    with err ->
      Printf.eprintfn "%s: %s" (token.loc.ToString()) err.Message
      exit 1

let parse (filePath : string) =
  Lexer.lexFile filePath |> Seq.map parseWord |> Blocks.resolve
