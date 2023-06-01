import Control.Exception (Handler (Handler), catches)
import MorthLanguage.Driver (BadUsage (..), CommandFailError (..), run)
import System.Exit (exitFailure)
import System.IO (stdout)

main :: IO ()
main =
  run stdout
    `catches` [ Handler (\CommandFailed -> exitFailure)
              , Handler (\BadUsage -> exitFailure)
              ]
