module MorthLanguage.Driver (CommandFailError (..), BadUsage (..), run) where

import Control.Exception (Exception, throw)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO
import Formatting (int, later, now, string, text, (%))
import MorthLanguage.Com (compileProgram)
import MorthLanguage.Logger (logCmd, logErr, logInfo)
import MorthLanguage.Parser (parseProgram)
import MorthLanguage.Sim (simulateProgram)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..))
import System.IO (Handle, hPutStrLn, stderr)
import System.Process (createProcess, proc, waitForProcess)
import Text.ShellEscape (Bash, Escape (bytes, escape))

data CommandFailError = CommandFailed deriving (Show)

instance Exception CommandFailError

data BadUsage = BadUsage deriving (Show)

instance Exception BadUsage

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
  logErr ("exit code " % int) n
  throw CommandFailed

runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
  logCmd text $
    TL.unwords $
      map
        ( TL.pack
            . B.unpack
            . bytes
            . (escape :: B.ByteString -> Bash)
            . B.pack
        )
        (cmd : args)
  (_, _, _, p) <- createProcess (proc cmd args)
  ec <- waitForProcess p
  check ec

run :: Handle -> IO ()
run hOut = do
  args <- getArgs
  case map TL.pack args of
    ["sim", path] -> do
      raw <- TLIO.readFile $ TL.unpack path
      program <- parseProgram (TL.toStrict path) raw
      simulateProgram hOut program
    ["sim"] -> do
      usage ()
      logErr text "no file given for 'sim'"
      throw BadUsage
    ["com", path] -> do
      raw <- TLIO.readFile $ TL.unpack path
      program <- parseProgram (TL.toStrict path) raw
      let asmText = compileProgram program
          asmPath = "output.asm"
          objPath = "output.o"
          exePath = "output"
       in do
            logInfo ("writing assembly to '" % string % "'") asmPath
            TLIO.writeFile asmPath asmText
            runCmd "nasm" ["-felf64", asmPath, "-o", objPath]
            runCmd "ld" ["-o", exePath, objPath]
            return ()
    ["com"] -> do
      usage ()
      logErr "no file given for 'com'"
      throw BadUsage
    ["help"] -> do
      usage ()
    [] -> do
      usage ()
      logErr "no subcommand given"
      throw BadUsage
    cmd : _ -> do
      usage ()
      logErr ("unknown subcommand '" % text % "'") cmd
      throw BadUsage
