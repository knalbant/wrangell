module DataParsers where
import CSV
import Text.ParserCombinators.Parsec
import DataTypes
import Control.Monad.Except

parseCSV :: Table -> String -> IOThrowsError WVal
parseCSV table toParseFile = do
    result <- liftIO $ parseFromFile (csvFile ',') toParseFile
    return Unit