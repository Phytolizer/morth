module Morth.Language.Parser

let parseWord (token : Token.t) =
  let code =
    (match token.word with
     | "+" -> Op.Plus
     | "-" -> Op.Minus
     | "=" -> Op.Eq
     | ">" -> Op.Gt
     | "dump" -> Op.Dump
     | "if" -> Op.If -1
     | "else" -> Op.Else -1
     | "while" -> Op.While
     | "do" -> Op.Do -1
     | "end" -> Op.End -1
     | "dup" -> Op.Dup
     | "mem" -> Op.Mem
     | "," -> Op.Load
     | "." -> Op.Store
     | "syscall1" -> Op.Syscall1
     | "syscall3" -> Op.Syscall3
     | n ->
       try
         Op.Push(int n)
       with err ->
         Printf.eprintfn "%s: %s" (token.loc.ToString()) err.Message
         exit 1) in

  new Op.t (token.loc, code)

let parse (filePath : string) =
  Lexer.lexFile filePath |> Seq.map parseWord |> Blocks.resolve
