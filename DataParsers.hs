module DataParsers where
import CSV
import Text.ParserCombinators.Parsec
import DataTypes
import Control.Monad.Except
import Data.IORef
import Data.Char
import Text.Read hiding (String)

fromRight :: Either a b -> b
fromRight (Right b) = b

maybeToTop :: Maybe WVal -> WVal
maybeToTop (Just w) = w
maybeToTop Nothing = Top

zipSingle :: a -> [b] -> [(a, b)]
zipSingle a [] = []
zipSingle a (b:bs) = (a, b) : zipSingle a bs


readMaybeInteger str = readMaybe fixedStr :: Maybe Integer
    where fixedStr = if last str == '.' then init str else str

readMaybeDouble str = readMaybe fixedStr :: Maybe Double
    where fixedStr = if last str == '.' then init str else str

parseTyped :: WType -> String -> Maybe WVal
parseTyped TString str = Just $ String str

parseTyped TBool boolStr
    | str `elem` ["true", "1", "yes"] = Just $ Bool True
    | str `elem` ["false", "0", "no"] = Just $ Bool False
    | otherwise                       = Nothing
    where str = map toLower boolStr

parseTyped TIntegral str = fmap (\x -> Integral x) (readMaybeInteger str)
parseTyped TFloat str = fmap (\x -> Float x) (readMaybeDouble str)



-- [IOThrowsError [Maybe WVal]]


parseTypedRow :: [WType] -> [String] -> IOThrowsError [WVal]
parseTypedRow types rows 
    | (length types == length rows) = return $ map maybeToTop $ map (uncurry parseTyped) (zip types rows)
    | otherwise = throwError $ CSVParseError $ "Inconsistent number of columns, " ++ show (length types) ++ " vs. " ++ show (length rows)

parseTypedStringTable :: [WType] -> [[String]] -> IOThrowsError [[WVal]]
parseTypedStringTable types strTable = sequenceA (map (parseTypedRow types) strTable)


parseCSVDelim :: Char -> Table -> String -> IOThrowsError WVal
parseCSVDelim delim table toParseFile = do
    result <- liftIO $ parseFromFile (csvFile delim) toParseFile
    case result of 
        Left err -> throwError $ CSVParseError (show err)
        Right _ -> return Unit

    let tableStrings = fromRight result
    table' <- liftIO $ readIORef table
    let tableTypes = format table'

    tableValues <- parseTypedStringTable tableTypes tableStrings
    let newTable' = table' { rows = tableValues }
    liftIO $ writeIORef table newTable'
    return Unit