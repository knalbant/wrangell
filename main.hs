module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import System.IO
import Numeric

import Parser
import Evaluation
import DataTypes
import Errors


readExpr :: String -> ThrowsError WVal
--parse is a function from parsec which Parsec
--the second argument is the parser to use
--the third argument is the name of the Parser
--and the fourth what the parser is applied to
--the return type is Either where
--Left signals an error and Right a proper parse
readExpr input = case parse parseExpr "wrangell" input of
  Left err  -> throwError $ Parser err
  Right val -> return val


--haskell flushes the output buffer on newlines so need a reliable way of
  --outputting to the screen when printing just a single string
flushString :: String -> IO ()
flushString str = putStr str >> hFlush stdout

--prints the prompt and reads in input
readPrompt :: String -> IO String
readPrompt prompt = flushString prompt >> getLine

evalString :: String -> IO String
evalString input =
  return . extractValue . trapError . liftM show $ readExpr input >>= eval

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

--do until loop where once pred evaluates to true we exit
doUntil_ :: Monad m => (a -> Bool) ->  m a -> (a -> m ()) -> m ()
doUntil_ predicate prompt action = do
  userInput <- prompt
  if predicate userInput then
    return ()
  else
    action userInput >> doUntil_ predicate prompt action


runRepl :: IO ()
runRepl = doUntil_ (== "quit") (readPrompt "wrangell>>> ") evalAndPrint

-- main = getArgs >>= (print . eval . readExpr . head)
main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint $ head args
            _ -> putStrLn "Usage ./wrangell expression -or- ./wrangell"
