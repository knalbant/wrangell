module DataWriters where
import DataTypes
import Data.IORef
import CSV

writeCSVDelim :: Char -> Table -> String -> IO ()
writeCSVDelim delim table name = do
    table' <- readIORef table
    let tableVals = rows table'
    let strVals = map (map show) tableVals
    let csvStr = genCsvFile delim strVals
    writeFile name csvStr