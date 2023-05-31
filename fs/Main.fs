module Morth.Main

open Morth.Language

[<EntryPoint>]
let main args =
  try
    Driver.run args stdout
  with Driver.BadUsage ->
    1
