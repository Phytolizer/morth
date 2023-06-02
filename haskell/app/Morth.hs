import Control.Exception (Handler (Handler), catches)
import Morth.Driver (run)
import Morth.Errors (BadUsage (..), CommandFailError (..), ParseError (..))
import System.Exit (exitFailure)
import System.IO (stdout)

main :: IO ()
main =
  run stdout
    `catches` [ Handler (\CommandFailed -> exitFailure)
              , Handler (\BadUsage -> exitFailure)
              , Handler (\ParseError -> exitFailure)
              ]
