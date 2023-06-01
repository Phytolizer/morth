module MorthLanguage.Logger (logCmd, logInfo, logErr) where

import System.IO (Handle, stderr)
import Text.Printf (HPrintfType, hPrintf, printf)

logCmd :: (HPrintfType r) => String -> r
logCmd = hPrintf stderr . printf "[CMD] %s\n"

logInfo :: (HPrintfType r) => String -> r
logInfo = hPrintf stderr . printf "[INFO] %s\n"

logErr :: (HPrintfType r) => String -> r
logErr = hPrintf stderr . printf "[ERROR] %s\n"
