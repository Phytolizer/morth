import MorthLanguage.Com (compileProgram)
import MorthLanguage.Logger (logCmd, logErr, logInfo)
import MorthLanguage.Op (Op (..))
import MorthLanguage.Parser (parseProgram)
import MorthLanguage.Sim (simulateProgram)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (IOMode (..), hClose, hPutStrLn, openFile, stderr, stdout)
import System.Process (
  CreateProcess (..),
  createProcess,
  proc,
  waitForProcess,
 )
import Text.Printf (hPrintf)

usage :: () -> IO ()
usage () = do
  progName <- getProgName
  mapM_
    (hPutStrLn stderr)
    [ "Usage: " ++ progName ++ " <SUBCOMMAND> [ARGS]"
    , "SUBCOMMANDS:"
    , "  sim <file>             Simulate a program"
    , "  com <file>             Compile a program"
    , "  help                   Print this message"
    ]

check :: ExitCode -> IO ()
check ExitSuccess = return ()
check (ExitFailure n) = do
  logErr "exit code %d" n
  exitFailure

run :: String -> [String] -> IO ()
run cmd args = do
  logCmd $ unwords (cmd : args)
  (_, _, _, p) <- createProcess (proc cmd args)
  ec <- waitForProcess p
  check ec

main :: IO ()
main = do
  args <- getArgs
  case args of
    "sim" : path : [] -> do
      raw <- readFile path
      simulateProgram stdout $ parseProgram raw
    "sim" : [] -> do
      usage ()
      logErr "no file given for 'sim'"
      exitFailure
    "com" : path : [] -> do
      raw <- readFile path
      let asmText = compileProgram $ parseProgram raw
          asmPath = "output.asm"
          objPath = "output.o"
          exePath = "output"
       in do
            logInfo "writing assembly to '%s'" asmPath
            writeFile asmPath asmText
            run "nasm" ["-felf64", asmPath, "-o", objPath]
            run "ld" ["-o", exePath, objPath]
            return ()
    "com" : [] -> do
      usage ()
      logErr "no file given for 'com'"
      exitFailure
    "help" : [] -> do
      usage ()
    [] -> do
      usage ()
      logErr "no subcommand given"
      exitFailure
    cmd : _ -> do
      usage ()
      logErr "unknown subcommand '%s'" cmd
      exitFailure
