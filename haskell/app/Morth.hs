import Control.Exception (catch)
import Morth.Driver (run)
import Morth.Errors (MorthError (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stdout)

handleErr :: MorthError -> IO ()
handleErr _ = exitFailure

main :: IO ()
main =
  (getArgs >>= run stdout)
    `catch` handleErr
