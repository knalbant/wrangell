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
setDelimiter env table delim = do
    unwrappedTable <- liftIO $ readIORef table
    liftIO $ writeIORef table (unwrappedTable {delimiter = delim})
    return $ Integral 0

-- setDelimiter' :: Table' -> String {}

dropColumn :: Env -> Table -> WVal -> IOThrowsError WVal
dropColumn env table (Integral index) = return $ Integral 0
dropColumn env table (String label) = return $ Integral 1
dropColumn env table val = throwError $ TypeError "Invalid type for dropColumn" val
