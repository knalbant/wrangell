module DataOperations where
import DataTypes
import Control.Monad.Except
import ArgParsing
import Data.IORef

formatTable :: Env -> Table -> [WVal] -> IOThrowsError WVal
formatTable env table formats = do

  --table <- liftIO $ readIORef table

  return $ Integral 0


dropColumn :: Env -> Table -> WVal -> IOThrowsError WVal
dropColumn env table (Integral index) = return $ Integral 0
dropColumn env table (String label) = return $ Integral 1
dropColumn env table val = throwError $ TypeError "Invalid type for dropColumn" val
