module Morth.Logger (
  logCmd,
  logInfo,
  logErr,
  logErrLoc,
) where

import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO
import Formatting (Format, formatted, prefixed)
import Morth.Location (Location, fmtLoc)
import System.IO (stderr)

out :: Format (IO ()) a -> a
out = formatted (TLIO.hPutStrLn stderr)

prefix :: Builder -> Format (IO ()) a -> a
prefix p f = out $ prefixed p f

logCmd :: Format (IO ()) a -> a
logCmd = prefix "[CMD] "

logInfo :: Format (IO ()) a -> a
logInfo = prefix "[INFO] "

logErr :: Format (IO ()) a -> a
logErr = prefix "[ERROR] "

logErrLoc :: Location -> Format (IO ()) a -> a
logErrLoc loc =
  prefix
    ( foldr
        mappend
        (TLB.fromText "")
        ["[ERROR] ", fmtLoc loc, ": "]
    )
