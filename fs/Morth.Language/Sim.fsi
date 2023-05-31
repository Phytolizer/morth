module Morth.Language.Sim

open System.IO

val simulate : TextWriter -> Op.t seq -> unit
