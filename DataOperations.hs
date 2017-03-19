module DataOperations where
import DataTypes
import Control.Monad.Except
import ArgParsing
import Data.IORef

formatTable :: Env -> Table -> [WVal] -> IOThrowsError WVal
formatTable env table formats = do

  --table <- liftIO $ readIORef table

  return $ Integral 0


setDelimiter :: Env -> Table -> String -> IOThrowsError WVal
setDelimiter env table delim = 
    doTableWrite env table (\e t -> (t {delimiter = delim}, Integral 0))

setLabels :: Env -> Table -> [WVal] -> IOThrowsError WVal
setLabels env table labels = doTableWrite env table (setLabels' labels)

setLabels' :: [WVal] -> Env -> Table' -> IOThrowsError (Table', WVal)
setLabels' wlabels env table = table

dropColumn :: Env -> Table -> WVal -> IOThrowsError WVal
dropColumn env table (Integral index) = return $ Integral 0
dropColumn env table (String label) = return $ Integral 1
dropColumn env table val = throwError $ TypeError "Invalid type for dropColumn" val



-- Helper function
doTableWrite :: Env -> Table -> (Env -> Table' -> IOThrowsError (Table', WVal)) -> IOThrowsError WVal
doTableWrite env table f = do
    unwrappedTable <- liftIO $ readIORef table
    let res = f env unwrappedTable 
    liftIO $ res >>= (\r -> writeIORef table (fst r))
    return $ ret



unpackAtomString :: WVal -> String
unpackAtomString (Atom s) = s