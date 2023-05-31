module Morth.Main

open Morth.Language

[<EntryPoint>]
let main args = Driver.run args stdout
