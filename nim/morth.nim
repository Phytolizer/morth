import op
import sim

const
  PROGRAM = [
    opPush(34),
    opPush(35),
    opPlus(),
    opDump(),
  ]

when isMainModule:
  simulateProgram(PROGRAM)
