import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (subcommand:[]) -> putStrLn subcommand
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <SUBCOMMAND> [ARGS]"
      exitFailure
