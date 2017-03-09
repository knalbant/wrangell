module Parser (parseExpr) where
import DataTypes
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.IO
import Data.Char
import Numeric

{-
  escapedChar ::= '\' ( '"' | 'n' | 'r' | 't' | '\' )
  parseString ::= '"' { escapedChar | - ( '"' | '\' ) } '"'
  parseAtom ::= ( letter | symbol ) { letter | synmbol | digit }
  parseBool ::= '#' ( 't' | 'f' )
  parseIntegral ::= digit+
  parseDecimal ::= [ '#d' ] parseIntegral
  parseHex ::= '#x' hexDigit+
  parseOct ::= '#o' octDigit+
  parseBin ::= '#b' ( '0' | '1' )+
  parseNumber ::= parseFloat | parseDecimal | parseHex | parseOct | parseBin
  parseFloat ::= digit+ '.' digit+
  parseListInternals ::= '' | ( parseExpr { spaces parseExpr} )
  parseQuoted ::= ''' parseExpr
  parseList ::= '(' parseListInternals ')'
  parseExpr ::= parseString | parseAtom | parseNumber | parseBool | parseQuoted | parseList
-}

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--returns a parser combinator which skips spaces
spaces :: Parser ()
spaces = skipMany1 space

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  res <- oneOf "\"nrt\\"
  return $ case res of
                '\\' -> res  --escaped backslash
                '"'  -> res  --escaped quote
                'n'  -> '\n' --escaped newline
                'r'  -> '\r' --escaped carriage return
                't'  -> '\t' --escaped quote

--intended to parse a string and return it as the proper wrangell type
parseString :: Parser WVal
parseString = do
  char '"' --strings start with double quotes
  x <- many $ escapedChar <|> noneOf "\"\\" --read until the closing quote mark
  char '"' --consume the closing quote mark
  return $ String x --wrap the string in a WVal string then in a parser context

--parse a WVal atom
parseAtom :: Parser WVal
parseAtom = do
  --the first character of an atom can be a symbol or letter
  first <- letter <|> symbol --first :: Char
  --the rest of the string can be any of letters, symbols, or digits
  rest <- many (letter <|> symbol <|> digit) --rest :: [Char] i.e. string
  --collect the results into one whole string
  let atom = first:rest
  --return wraps the result of the expression into a Parser context
  return $ Atom atom

parseBool :: Parser WVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseIntegral :: Parser WVal
parseIntegral = do
  strNum <- many1 digit --an integral is a non-empty sequence of digits
  return $ Integral $ read strNum

parseDecimal :: Parser WVal
parseDecimal = try (string "#d" >> parseIntegral) <|> parseIntegral

parseHex :: Parser WVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  --readhex returns its value as a [(String,String)] so use fst. head to get at it
  return $ (Integral . fst . head . readHex) x

parseOct :: Parser WVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ (Integral . fst . head . readOct) x

bin2dec :: String -> Integer
bin2dec = foldr (\c acc -> acc * 2 + (toInteger . digitToInt) c ) 0 . reverse

parseBin :: Parser WVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "01")
  return $ (Integral . bin2dec) x

parseFloat :: Parser WVal
parseFloat = do
  whole <- many1 digit
  char '.'
  frac <- many1 digit
  let floatStr = whole ++ "." ++ frac
  return $ (Float . fst . head . readFloat) floatStr

parseNumber :: Parser WVal
parseNumber = try parseFloat <|> parseDecimal <|> parseHex <|> parseOct <|> parseBin

--parses the internals of a list but not the parens around it
--just in case we want to add support for dotted lists later
parseListInternals :: Parser WVal
parseListInternals = liftM List $ sepBy parseExpr spaces

parseQuoted :: Parser WVal
parseQuoted = do
  char '\''
  res <- parseExpr
  return $ List [Atom "quote", res]

parseList :: Parser WVal
parseList = do
  char '('
  res <- parseListInternals
  char ')'
  return res

parseExpr :: Parser WVal
parseExpr = parseString
         <|> parseAtom
         <|> try parseNumber
         <|> try parseBool
         <|> parseQuoted
         <|> parseList
