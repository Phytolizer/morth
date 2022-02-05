import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Op
  = OpPush Int
  | OpPlus
  | OpMinus
  | OpDump
  deriving (Show)

pop :: [Int] -> (Int, [Int])
pop [] = error "stack underflow"
pop (x:rest) = (x, rest)

simulateProgram :: [Op] -> IO ()
simulateProgram program = do
  let loop ip stack = do
        when (ip < length program) $ do
          putStrLn $ " -- " ++ show (program !! ip) ++ " --"
          case program !! ip of
            OpPush x -> do
              let stack' = (x : stack)
                  ip' = ip + 1
               in loop ip' stack'
            OpPlus -> do
              let (y, stack') = pop stack
                  (x, stack'') = pop stack'
                  stack3 = (x + y : stack'')
                  ip' = ip + 1
               in loop ip' stack3
            OpMinus -> do
              let (y, stack') = pop stack
                  (x, stack'') = pop stack'
                  stack3 = (x - y : stack'')
                  ip' = ip + 1
               in loop ip' stack3
            OpDump -> do
              let (x, stack') = pop stack
                  ip' = ip + 1
               in do print x
                     loop ip' stack'
   in loop 0 []

compileProgram :: [Op] -> IO ()
compileProgram = error "TODO"

program :: [Op]
program =
  [OpPush 34, OpPush 35, OpPlus, OpDump, OpPush 500, OpPush 80, OpMinus, OpDump]

usage :: IO ()
usage = do
  name <- getProgName
  hPutStrLn stderr $ "usage: " ++ name ++ " <SUBCOMMAND> [ARGS]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [subcommand] ->
      case subcommand of
        "sim" -> simulateProgram program
        "com" -> compileProgram program
        _ -> do
          usage
          exitFailure
    _ -> do
      usage
      exitFailure
