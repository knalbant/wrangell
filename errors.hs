module Errors where
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

import DataTypes

data WError = Parser ParseError
    | NotFunction FuncDef deriving (Show)

showError :: WError -> String
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (NotFunction funcDef) = "Could not find function: " ++ show funcDef

type ThrowsError = Either WError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val