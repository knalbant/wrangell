module DataWriters where
import DataTypes
import Data.IORef
import CSV

writeCSVDelim :: Char -> Table -> String -> IO ()
writeCSVDelim delim table name = do
    table' <- readIORef table
    let tableVals = rows table'
    let strVals = map (map showp) tableVals
    let csvStr = genCsvFile delim strVals
    writeFile name csvStr

--quite possibly the dirtiest hack I'll ever be responsible for
showp :: WVal -> String
showp (String s) = '!':s
showp val = show val
