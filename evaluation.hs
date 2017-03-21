--module Evaluation (eval) where
module Evaluation where
import Control.Monad.Except
import Data.Maybe
import Data.List

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

eval env table (List [Atom "print", val]) = do
  res <- fmap (show) $ eval env table val
  liftIO $ putStrLn res
  return Unit

eval env table (List (Atom "print":rest)) = throwError $ NumArgs 1 rest

eval env table (List [Atom "define", Atom var, form]) =
      eval env table form >>= defineVar env var
eval env _ (List (Atom "define" : List (Atom var : params) : body)) = do
      makeFunc env params body >>= defineVar env var
eval env _ (List (Atom "lambda" : List params : body)) =
      makeFunc env params body


eval env table (List [Atom "dropColumn", Integral index]) =
  checkFormatDefined table >> dropColumnIndex env table index
eval env table (List [Atom "dropColumn", Atom label]) =
  checkFormatDefined table >> dropColumnLabel env table label
eval env table (List (Atom "dropColumn":rest)) =
  throwError $ TableOperation "dropColumn" "atom|integral" rest

eval env table (List [Atom "transformColumn", Integral index, f]) =
  checkFormatDefined table >> transformColumnIndex env table index f
eval env table (List [Atom "transformColumn", Atom label, f]) =
  checkFormatDefined table >> transformColumnLabel env table label f


eval env table (List [Atom "printTable"]) = do
  unWrappedTable <- liftIO $ fmap rows $readIORef table

  let wtf = (++ "\n") . concat . intersperse "\n" . map (unwords . map show)

  liftIO $ putStrLn $ wtf unWrappedTable

  return Unit



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

--gonna need to call this before
checkFormatDefined :: Table -> IOThrowsError WVal
checkFormatDefined table = do
  unWrappedTable <- liftIO $ readIORef table
  if null $ format unWrappedTable then throwError FormatNotDefined
                                  else return Unit



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
