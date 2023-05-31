module Morth.Language.Driver

open System.IO

val run : string array -> TextWriter -> int
exception BadUsage
