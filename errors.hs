module Errors where
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

import DataTypes

data WError = NumArgs Integer [WVal]
    | TypeMismatch WType WVal
    | Parser ParseError
    | NotFunction FuncDef

showError :: WError -> String
showError (NumArgs expected found) = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Expected type " ++ show expected ++ "; found value " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (NotFunction funcDef) = "Could not find function: " ++ show funcDef

type ThrowsError = Either WError
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val