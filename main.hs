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



main :: IO ()
main = do
    args <- getArgs
    let evaled = liftM show $ readExpr (args !! 0) >>= eval
    (putStrLn . extractValue . trapError) evaled 
