--module Evaluation (eval) where
module Evaluation where
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
apply func args =
    if isNothing funcDef
      then throwError $ NotFunction "Unrecognized primitive function args" func
      else fromJust funcDef args
    where funcDef = lookup func funcTable
