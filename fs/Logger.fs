module Morth.Logger

let info x = Printf.kprintf (printfn "[INFO] %s") x
let cmd x = Printf.kprintf (printfn "[CMD] %s") x

let error x =
  Printf.ksprintf (eprintfn "[ERROR] %s") x
