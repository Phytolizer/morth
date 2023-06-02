module Morth.Driver (run) where

import Control.Exception (throw)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Functor ((<&>))
import Data.Primitive (arrayFromList, unsafeThawArray)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Formatting (int, string, text, (%))
import Morth.Blocks (resolveBlocks)
import Morth.Com (compileProgram)
import Morth.Errors (BadUsage (BadUsage), CommandFailError (..))
import Morth.Logger (logCmd, logErr, logInfo)
import Morth.Parser (parseProgram)
import Morth.Sim (simulateProgram)
import System.Environment (getProgName)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, (-<.>), (</>))
import System.IO (Handle, hPutStrLn, stderr, stdout)
import System.Process (
  CreateProcess (std_out),
  StdStream (UseHandle),
  createProcess,
  proc,
  waitForProcess,
 )
import Text.ShellEscape (Bash, Escape (bytes, escape))

usage :: () -> IO ()
usage () = do
  progName <- getProgName
  mapM_
    (hPutStrLn stderr)
    [ "Usage: " ++ progName ++ " <SUBCOMMAND> [ARGS]"
    , "SUBCOMMANDS:"
    , "  sim <file>                   Simulate a program"
    , "  com [-r] <file>              Compile a program"
    , "  help                         Print this message"
    ]

check :: ExitCode -> IO ()
check ExitSuccess = return ()
check (ExitFailure n) = do
  logErr ("exit code " % int) n
  throw CommandFailed

captureCmd :: Handle -> String -> [String] -> IO ()
captureCmd hOut cmd args = do
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
  (_, _, _, p) <- createProcess (proc cmd args){std_out = UseHandle hOut}
  ec <- waitForProcess p
  check ec

runCmd :: String -> [String] -> IO ()
runCmd = captureCmd stdout

parseComArgs :: [TL.Text] -> IO (Bool, TL.Text)
parseComArgs args = loop args False
 where
  loop [] _ = do
    usage ()
    logErr "no file given for 'com'"
    throw BadUsage
  loop (arg : args') shouldRun = case arg of
    "-r" -> loop args' True
    path -> return (shouldRun, path)

toRelative :: FilePath -> FilePath
toRelative path
  | isAbsolute path = path
  | otherwise = "." </> path

run :: Handle -> [String] -> IO ()
run hOut args = do
  case map TL.pack args of
    ["sim", path] -> do
      raw <- TLIO.readFile $ TL.unpack path
      program <-
        parseProgram (TL.toStrict path) raw
          >>= unsafeThawArray . arrayFromList
          >>= resolveBlocks

      simulateProgram hOut program
    ["sim"] -> do
      usage ()
      logErr text "no file given for 'sim'"
      throw BadUsage
    ("com" : comArgs) -> do
      (shouldRun, path) <- parseComArgs comArgs
      raw <- TLIO.readFile $ TL.unpack path
      asmText <-
        parseProgram (TL.toStrict path) raw
          >>= unsafeThawArray . arrayFromList
          >>= resolveBlocks
          <&> compileProgram
      let asmPath = TL.unpack path -<.> "asm"
          objPath = TL.unpack path -<.> "o"
          exePath = TL.unpack path -<.> ""
       in do
            logInfo ("writing assembly to '" % string % "'") asmPath
            TLIO.writeFile asmPath asmText
            runCmd "nasm" ["-felf64", asmPath, "-o", objPath]
            runCmd "ld" ["-o", exePath, objPath]
            when shouldRun $
              captureCmd hOut (toRelative exePath) []
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
