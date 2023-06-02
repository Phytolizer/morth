module MorthLanguage.Logger (
  logCmd,
  logInfo,
  logErr,
  logErrLoc,
) where

import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.IO as TLIO
import Formatting (Format, bformat, formatted, later, prefixed, (%))
import MorthLanguage.Token (Location (..), fmtLoc)
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

logErrLoc :: Location -> Format Builder (a -> Builder) -> a -> IO ()
logErrLoc loc x =
  logErr $ later $ bformat (fmtLoc % ": " % x) loc
