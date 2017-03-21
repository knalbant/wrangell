module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import System.IO
import Numeric

import Parser
import Evaluation
import Functions
import DataTypes
import DataOperations


readOrThrow :: Parser a -> String -> ThrowsError a
--parse is a function from parsec which Parsec
--the second argument is the parser to use
--the third argument is the name of the Parser
--and the fourth what the parser is applied to
--the return type is Either where
--Left signals an error and Right a proper parse
readOrThrow parser input = case parse parser "wrangell" input of
  Left err  -> throwError $ Parser err
  Right val -> return val


readExpr     = readOrThrow parseExpr
readExprList = readOrThrow (sepEndBy parseExpr spacesNewLines)


--haskell flushes the output buffer on newlines so need a reliable way of
  --outputting to the screen when printing just a single string
flushString :: String -> IO ()
flushString str = putStr str >> hFlush stdout

--prints the prompt and reads in input
readPrompt :: String -> IO String
readPrompt prompt = flushString prompt >> getLine

evalString :: Env -> Table -> String -> IO String
evalString env table input =
  runIOThrows $ liftM show $ (liftThrows $ readExpr input) >>= eval env table

--injects our builtins into defined labels
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc funcTable)
  where makePrimitiveFunc (name, func) = (name, BuiltIn func)


--helper function for running a single expression from the CL
runOne :: String -> IO ()
runOne expr = do
  binds <- primitiveBindings
  table <- emptyTable
  evalAndPrint binds table expr

evalAndPrint :: Env -> Table -> String -> IO ()
evalAndPrint env table expr = evalString env table expr >>= putStrLn

--do until loop where once pred evaluates to true we exit
doUntil_ :: Monad m => (a -> Bool) ->  m a -> (a -> m ()) -> m ()
doUntil_ predicate prompt action = do
  userInput <- prompt
  if predicate userInput then
    return ()
  else
    action userInput >> doUntil_ predicate prompt action


load :: String -> IOThrowsError [WVal]
load filename = do
  fStr <- liftIO (readFile filename) -- >>= liftThrows . readExprList

  wvals <- (liftThrows . readExprList) fStr
  --liftIO (mapM (putStrLn . show) wvals)

  return wvals


--load :: [String] -> IO ThrowsError [WVal]
--load filename = readFile filename >>= return . readExprList


--evalSeq :: IOThrowsError [WVal] -> WVal
--evalSeq

runFile :: String -> Env -> Table -> IO ()
runFile filename env table = do
  {--
  let wArgs = drop 1 args
  let filename = head args
  table <- emptyTable
  bindings <- primitiveBindings

  env <- bindVars bindings $ [("args", List $ map String wArgs)]
  --}
  (runIOThrows $ liftM show $ load filename >>= eval env table . Seq) >>= putStrLn
  writeTable table


runRepl :: Env -> Table -> IO ()
runRepl env table = do
  {--
  binds <- primitiveBindings
  table <- emptyTable
  --}
  doUntil_ (== "quit") (readPrompt "wrangell>>> ") (evalAndPrint env table)

--might want a runReplPrimitive where it's just a plain repl

runWrangell :: [String] -> IO ()
runWrangell args = do
  let wArgs = if length args == 1 then args else drop 1 args
  let filename = head args
  table <- emptyTable
  bindings <- primitiveBindings

  env <- bindVars bindings $ [("args", List $ map String wArgs)]

  runWrangellHelp filename args env table

  where runWrangellHelp filename args env table
         | length args == 1 = runRepl env table
         | length args == 3 = runFile filename env table
         | otherwise        = error commandLineErrString  --TODO: change to the way in proper CLI tools (defined in datatypes.hs)

main = do args <- getArgs
          runWrangell args
