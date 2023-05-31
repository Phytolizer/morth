module Morth.Language.Lexer

open System
open System.IO

let findCol (line : string) start pred =
  let rec loop i =
    if i >= line.Length then line.Length
    elif pred line.[i] then i
    else loop (i + 1) in

  loop start

let lexLine filePath lineNum line =
  let rec loop start =
    let col = findCol line start (fun c -> not (Char.IsWhiteSpace(c))) in

    if col >= line.Length then
      Seq.empty
    else
      let endCol = findCol line col (fun c -> Char.IsWhiteSpace(c)) in
      let word = line.Substring(col, endCol - col) in
      let loc = new Token.loc (filePath, lineNum, col + 1) in
      let token = new Token.t (loc, word) in
      Seq.append (Seq.singleton token) (loop endCol) in

  loop 0

let lexFile filePath =
  let f = File.OpenText(filePath) in

  let rec loop lineNum =
    match f.ReadLine() with
    | null -> Seq.empty
    | line ->
      let line = line.Split("//").[0] in
      Seq.append (lexLine filePath lineNum line) (loop (lineNum + 1)) in

  loop 1
