module Errors where
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

{--
import DataTypes

data WError = Parser ParseError
    | NotFunction FuncDef

type IOThrowsError = ExceptT WError IO

instance Show WError where show = showError

showError :: WError -> String
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (NotFunction funcDef) = "Could not find function: " ++ show funcDef

type ThrowsError = Either WError

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

trapError action = catchError action (return . show)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
--}
