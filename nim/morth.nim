import op
import sim

const
  PROGRAM = [
    opPush(34),
    opPush(35),
    opPlus(),
    opDump(),
    opPush(500),
    opPush(80),
    opMinus(),
    opDump(),
  ]

when isMainModule:
  simulateProgram(PROGRAM)
