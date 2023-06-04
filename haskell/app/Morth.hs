import Control.Exception (catch)
import Morth.Driver (run)
import Morth.Errors (MorthError (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith)

handleErr :: MorthError -> IO a
handleErr _ = exitFailure

main :: IO ()
main =
  exitWith
    =<< (getArgs >>= run)
      `catch` handleErr
