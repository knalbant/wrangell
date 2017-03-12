module Evaluation (eval) where
import Control.Monad.Except
import Data.Maybe

import DataTypes
import Errors
import Functions

eval :: WVal -> ThrowsError WVal
eval val@(String _) = return val
eval val@(Integral _) = return val
eval val@(Bool _) = return val
eval val@(Float _) = return val
eval (List [Atom "quote", l]) = return l
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [WVal] -> ThrowsError WVal
apply func args = if isNothing foundFunc
    then throwError badFuncError
    else return ((fromJust foundFunc) args)
    where types = convertListOp $ map getType args
          badFuncError = NotFunction (func, types)
          foundFunc = lookup (func, types) funcTable


--a hacky fix to have wrangell work with lists of the same type
convertListOp :: [WType] -> [WType]
convertListOp typeList =
  if length typeList > 2 then
    if all (== (head typeList)) typeList then take 2 typeList else typeList
  else typeList
