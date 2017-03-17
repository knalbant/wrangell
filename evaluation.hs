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

eval env (List (Atom "define" : List (Atom var : params) : body)) = do
          makeFunc env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
          makeFunc env params body
eval env (List (fname : args)) = do --mapM (eval env) args >>= liftThrows . apply func
  func    <- eval env fname    --get the function
  --liftIO $ putStrLn $ show fname ++ "in eval"
  liftIO $ putStrLn $ show func ++ "in eval"
  argVals <- mapM (eval env) args --get the arguments
  apply func argVals


{-- apply :: String -> [WVal] -> ThrowsError WVal
apply func args =
    if isNothing funcDef
      then throwError $ NotFunction "Unrecognized primitive function args" func
      else fromJust funcDef args
    where funcDef = lookup func funcTable
--}

apply :: WVal -> [WVal] -> IOThrowsError WVal
apply (BuiltIn func) args = liftThrows $ func args
apply (Func params body closure) args = do
  liftIO $ putStrLn $ unwords $ map show params
  liftIO $ putStrLn $ unwords $ map show args
  liftIO $ putStrLn $ unwords $ map show body
  --liftIO $ putStrLn $ show closure
  if num params /= num args then throwError $ NumArgs (num params) args
                            else captureVars >>= evalBody

  where num = toInteger . length --neccesary as length returns and Int not Integer
        captureVars  = liftIO $ bindVars closure $ zip params args
        evalBody env = liftM last $ mapM (eval env) body

--apply val l = do  liftIO $ putStrLn $ show val ++ "in catchall"
                  --return $ Integral 2
