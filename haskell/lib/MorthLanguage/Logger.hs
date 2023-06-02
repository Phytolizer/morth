module MorthLanguage.Logger (
  logCmd,
  logInfo,
  logErr,
  logErrLoc,
) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.IO (hPutStrLn)
import qualified Data.Text.Lazy.IO as TLIO
import Formatting (Format (runFormat), bformat, formatted, hprintLn, int, later, prefixed, text, (%))
import MorthLanguage.Token (Location (..), fmtLoc)
import System.IO (Handle, stderr)

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
