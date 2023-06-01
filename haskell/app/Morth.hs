import MorthLanguage.Com (compileProgram)
import MorthLanguage.Logger (logCmd, logErr, logInfo)
import MorthLanguage.Op (Op (..))
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

program :: [Op]
program =
  [ OpPush 34
  , OpPush 12
  , OpPlus
  , OpDump
  , OpPush 78
  , OpPush 56
  , OpMinus
  , OpDump
  ]

usage :: () -> IO ()
usage () = do
  progName <- getProgName
  mapM_
    (hPutStrLn stderr)
    [ "Usage: " ++ progName ++ " <SUBCOMMAND> [ARGS]"
    , "SUBCOMMANDS:"
    , "  sim            Simulate a program"
    , "  com            Compile a program"
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
    [] -> do
      usage ()
      logErr "no subcommand given"
      exitFailure
    "sim" : [] -> do
      simulateProgram stdout program
    "com" : [] ->
      let asmText = compileProgram program
          asmPath = "output.asm"
          objPath = "output.o"
          exePath = "output"
       in do
            logInfo "writing assembly to '%s'" asmPath
            hOut <- openFile asmPath WriteMode
            hPutStrLn hOut asmText
            hClose hOut
            run "nasm" ["-felf64", asmPath, "-o", objPath]
            run "ld" ["-o", exePath, objPath]
            return ()
    cmd : _ -> do
      usage ()
      logErr "unknown subcommand '%s'" cmd
      exitFailure
