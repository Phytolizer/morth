module Morth.Parser

let parseWord (token : Token.t) =
  match token.word with
  | "+" -> Op.Plus
  | "-" -> Op.Minus
  | "=" -> Op.Eq
  | ">" -> Op.Gt
  | "." -> Op.Dump
  | "if" -> Op.If -1
  | "else" -> Op.Else -1
  | "while" -> Op.While
  | "do" -> Op.Do -1
  | "end" -> Op.End -1
  | "dup" -> Op.Dup
  | n ->
    try
      Op.Push(int n)
    with err ->
      Printf.eprintfn "%s: %s" (token.loc.ToString()) err.Message
      exit 1

let parse (filePath : string) =
  Lexer.lexFile filePath |> Seq.map parseWord |> Blocks.resolve
