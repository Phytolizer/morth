module Morth.Token

type loc =
  struct
    val filePath : string
    val line : int
    val column : int

    new(filePath, line, column) =
      {
        filePath = filePath
        line = line
        column = column
      }

    override this.ToString() =
      sprintf "%s:%d:%d" this.filePath this.line this.column
  end

type t =
  struct
    val loc : loc
    val word : string

    new(loc, word) = { loc = loc; word = word }
  end
