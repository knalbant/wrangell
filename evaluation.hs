module Evaluation (eval) where
import Control.Monad.Except
import Data.Maybe

import DataTypes
import Errors
import Functions

eval :: Env -> WVal -> IOThrowsError WVal
eval env val@(String _) = return val
eval env val@(Integral _) = return val
eval env val@(Bool _) = return val
eval env val@(Float _) = return val
eval env     (Atom id) = getVar env id
eval env (List [Atom "quote", l]) = return l
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func

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
