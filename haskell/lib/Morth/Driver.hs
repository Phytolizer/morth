{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Morth.Driver (run) where

import Control.Exception (throw)
import qualified Data.ByteString.Char8 as B
import Data.Functor ((<&>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Formatting (int, string, text, (%))
import Morth.Compiler (Target (NasmX86_64), compileProgram)
import Morth.Errors (MorthError (BadUsage, CommandFailed))
import Morth.Frontend (readProgram)
import Morth.Logger (logCmd, logErr, logInfo)
import Morth.OS (OS (Linux))
import Morth.Interpreter (ByteOrder (LittleEndian), interpretProgram)
import System.Environment (getProgName)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, (-<.>), (<.>), (</>))
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
    , "  com [OPTIONS] <file>         Compile a program"
    , "    OPTIONS:"
    , "      -r                       Run the compiled program"
    , "      -o <file>                Output file"
    , "  help                         Print this message"
    ]

check :: ExitCode -> IO ()
check ExitSuccess = return ()
check (ExitFailure n) = do
  logErr ("exit code " % int) n
  throw CommandFailed

captureCmd :: Handle -> String -> [String] -> IO ExitCode
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
  waitForProcess p

runCmd :: String -> [String] -> IO ()
runCmd cmd args = captureCmd stdout cmd args >>= check

data ComArgs = ComArgs
  { comShouldRun :: Bool
  , comOutPath :: Maybe TL.Text
  , comInput :: TL.Text
  }

newtype Conf = Conf
  { confIncludePaths :: [FilePath]
  }

defaultIncludePath :: [FilePath]
defaultIncludePath = [".", "std"]

parseArgs :: [TL.Text] -> IO ([TL.Text], Conf)
parseArgs args = loop args (Conf defaultIncludePath)
 where
  loop :: [TL.Text] -> Conf -> IO ([TL.Text], Conf)
  loop [] conf = return ([], conf)
  loop ("-I" : path : args') conf =
    loop
      args'
      conf{confIncludePaths = TL.unpack path : confIncludePaths conf}
  loop ["-I"] _ = do
    logErr "no path given for '-I'"
    throw BadUsage
  loop passthroughArgs conf = return (passthroughArgs, conf)

parseComArgs :: [TL.Text] -> IO ([TL.Text], ComArgs)
parseComArgs args = loop args False Nothing
 where
  loop :: [TL.Text] -> Bool -> Maybe TL.Text -> IO ([TL.Text], ComArgs)
  loop [] _ _ = do
    usage ()
    logErr "no file given for 'com'"
    throw BadUsage
  loop rest sr op = case rest of
    ("-r" : args') -> loop args' True op
    ("-o" : outPath : args') -> loop args' sr (Just outPath)
    (path : passthroughArgs) -> return (passthroughArgs, ComArgs sr op path)

toRelative :: FilePath -> FilePath
toRelative path
  | isAbsolute path = path
  | otherwise = "." </> path

run :: [String] -> IO ExitCode
run args = do
  (args', conf) <- parseArgs $ TL.pack <$> args
  case args' of
    ["sim", path] -> do
      raw <- TLIO.readFile $ TL.unpack path
      readProgram (confIncludePaths conf) (TL.toStrict path) raw
        >>= interpretProgram LittleEndian Linux stdout
      return ExitSuccess
    ["sim"] -> do
      usage ()
      logErr text "no file given for 'sim'"
      throw BadUsage
    ("com" : comArgs) -> do
      ( passthroughArgs
        , ComArgs
            { comShouldRun = shouldRun
            , comOutPath = outPath
            , comInput = input
            }
        ) <-
        parseComArgs comArgs
      raw <- TLIO.readFile $ TL.unpack input
      asmText <-
        readProgram (confIncludePaths conf) (TL.toStrict input) raw
          <&> compileProgram NasmX86_64 Linux
      let outPath' = maybe (TL.unpack input -<.> "") TL.unpack outPath
          asmPath = outPath' <.> "asm"
          objPath = outPath' <.> "o"
          exePath = outPath'
       in do
            logInfo ("writing assembly to '" % string % "'") asmPath
            TLIO.writeFile asmPath asmText
            runCmd "nasm" ["-felf64", asmPath, "-o", objPath]
            runCmd "ld" ["-o", exePath, objPath]
            if shouldRun
              then captureCmd stdout (toRelative exePath) (map TL.unpack passthroughArgs)
              else return ExitSuccess
    ["help"] -> do
      usage ()
      return ExitSuccess
    [] -> do
      usage ()
      logErr "no subcommand given"
      throw BadUsage
    cmd : _ -> do
      usage ()
      logErr ("unknown subcommand '" % text % "'") cmd
      throw BadUsage
