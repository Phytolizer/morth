module Morth.Language.Logger

let info x =
  Printf.ksprintf (eprintfn "[INFO] %s") x

let cmd x = Printf.ksprintf (eprintfn "[CMD] %s") x

let error x =
  Printf.ksprintf (eprintfn "[ERROR] %s") x

let locError (loc : Token.loc) x =
  Printf.ksprintf (eprintfn "[ERROR] %s: %s" (loc.ToString())) x
