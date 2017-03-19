module DataOperations where
import DataTypes
import Control.Monad.Except
import ArgParsing

formatTable :: Env -> Table -> String -> [WVal] -> IOThrowsError WVal
formatTable env table filetype formats = return $ Integral 0
    where inFileName = getParsedArgs env >>= getInputFile


setDelimiter :: Env -> Table -> String -> IOThrowsError WVal
setDelimiter env table delimiter = 

dropColumn :: Env -> Table -> WVal -> IOThrowsError WVal
dropColumn env table (Integral index) = return $ Integral 0
dropColumn env table (String label) = return $ Integral 1
dropColumn env table val = throwError $ TypeError "Invalid type for dropColumn" val

