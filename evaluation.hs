--module Evaluation (eval) where
module Evaluation where
import Control.Monad.Except
import Data.Maybe

import DataTypes
import Errors
import Functions
import DataOperations

import Data.IORef


eval :: Env -> Table -> WVal -> IOThrowsError WVal
eval env _ val@(String _) = return val
eval env _ val@(Integral _) = return val
eval env _ val@(Bool _) = return val
eval env _ val@(Float _) = return val
eval env _    (Atom id) = getVar env id
eval env _ (List [Atom "quote", l]) = return l

eval env table (List [Atom "if", cond, t, f]) = do
  res <- eval env table cond
  if unpackBool res
    then eval env table t
    else eval env table f


eval env table (List [Atom "define", Atom var, form]) =
      eval env table form >>= defineVar env var
eval env _ (List (Atom "define" : List (Atom var : params) : body)) = do
      makeFunc env params body >>= defineVar env var
eval env _ (List (Atom "lambda" : List params : body)) =
      makeFunc env params body

eval env table (List [Atom "dropColumn", val]) = dropColumn env table val

eval env table (List [Atom "delimiter", String delimiter]) =
  throwError $ NotImplemented "Delimiters not currently supported"--setDelimiter env table delimiter
eval env table (List (Atom "delimiter":rest)) =
  throwError $ DelimFormat rest

eval env table (List ((Atom "labels") : labels)) =
  setLabels env table labels

--as soon as we've read in the format we can being parsing the file
eval env table (List ((Atom "formatTable") : formats)) = do
      formatTable env table formats
      result <- parseFile env table
      return Unit 

eval env table (List (fname : args)) = do --mapM (eval env) args >>= liftThrows . apply func
      func    <- eval env table fname    --get the function
      argVals <- mapM (eval env table) args --get the arguments
      apply table func argVals

eval env table (Seq [])  = return Unit --just in case
eval env table (Seq [l]) = eval env table l
eval env table (Seq (c:rest)) = do
  eval env table c
  eval env table $ Seq rest


apply :: Table -> WVal -> [WVal] -> IOThrowsError WVal
apply table (BuiltIn func) args = liftThrows $ func args
apply table (IOFunc func)  args = func args
apply table (Func params body closure) args = do
  if num params /= num args then throwError $ NumArgs (num params) args
                            else captureVars >>= evalBody

  where num = toInteger . length --neccesary as length returns and Int not Integer
        captureVars  = liftIO $ bindVars closure $ zip params args
        evalBody env = liftM last $ mapM (eval env table) body

apply _ val _ = throwError $ NotFunction "Attempted to call non-function" $ show val
