module Main where
import Parser
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.IO
import Numeric



readExpr :: String -> String
--parse is a function from parsec which Parsec
--the second argument is the parser to use
--the third argument is the name of the Parser
--and the fourth what the parser is applied to
--the return type is Either where
--Left signals an error and Right a proper parse
readExpr input = case parse parseExpr "wrangell" input of
  Left err  ->  "No parse: " ++ show err
  Right val -> "Found val: " ++ show val



main :: IO ()
main = getArgs >>= (putStrLn . readExpr . head)
