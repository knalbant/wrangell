
module DataOperations where
import DataTypes
import Control.Monad.Except
import ArgParsing
import Data.IORef
import Data.Char
import Data.Maybe
import Data.List
import CSV
import DataParsers
import DataWriters
import Control.Monad
import {-# SOURCE #-} Evaluation

allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

typeTable :: [(String, WType)]
typeTable = [
            ("int", TIntegral),
            ("integer", TIntegral),
            ("double", TFloat),
            ("float", TFloat),
            ("string", TString),
            ("boolean", TBool),
            ("bool", TBool)
            ]

--modify this and parsefile to allow for different filetypes
fileExtensions :: [(String, FileType)]
fileExtensions = [
                 (".csv", CSV),
                 (".tsv", TSV)
                 --(".txt", Text)
                 ]

--as more filetypes are supported add the parsing functions here
fileParsers :: [(FileType, Table -> String -> IOThrowsError WVal)]
fileParsers =
            [
            (CSV, parseCSVDelim ','),
            (TSV, parseCSVDelim '\t')
            ]

fileWriters :: [(FileType, Table -> String -> IO ())]
fileWriters =
            [
            (CSV, writeCSVDelim ','),
            (TSV, writeCSVDelim '\t')
            ]


checkLengthFormats :: Table' -> [WVal] -> IOThrowsError WVal
checkLengthFormats table formats
  | null formats =
    throwError $
      FormatSpec "Formats should have at least one element found: " formats

  | not (null (format table)) &&
      length formats /= length (labels table)
    =
    throwError $
      FormatSpec "Formats should have the same length as labels found: " formats

  | otherwise = return Unit

checkLengthLabels :: Table' -> [WVal] -> IOThrowsError WVal
checkLengthLabels table labels
  | null labels =
    throwError $
      FormatSpec "Labels should have at least one element found: " labels

  | not (null (format table)) &&
      length labels /= length (format table)
    =
    throwError $
      FormatSpec "Labels should have the same length as formats found: " labels

  | otherwise = return Unit

checkAllAtoms :: [WVal] -> IOThrowsError WVal
checkAllAtoms formats = if all (==TAtom) $ map getType formats
                        then return Unit
                        else throwError $ FormatSpec "Formats should be a list of all atoms found: " formats

checkAllUnique :: [String] -> [WVal] -> IOThrowsError WVal
checkAllUnique labels wlabels = if allUnique labels
                        then return $ Unit --basically just a dummy value
                        else throwError $ FormatSpec "Labels should be all unique: " wlabels


formatTable :: Env -> Table -> [WVal] -> IOThrowsError WVal
formatTable env table formats = do
  unWrappedTable <- liftIO $ readIORef table

  --prechecks
  checkLengthFormats unWrappedTable formats
  checkAllAtoms formats


  let formatStrList = atomList2StrList formats

  --returns Nothing if an invalid type specifier is found otherwise returns a Just [WType]
  let res = sequenceA $ map (flip lookup typeTable) formatStrList

  if isJust res
    then do
      let modTable = formatHelp unWrappedTable $ fromJust res
      liftIO $ writeIORef table modTable
      return Unit
    else
      throwError $
      FormatSpec ("Format specifiers are one of " ++
                  unwords (map fst typeTable) ++ " found: ") formats

        --returns a new table with format filled in
  where formatHelp tab formatList = tab { format = formatList  }
        --turns a list of atoms into a list of strings while also ignoring case
        atomList2StrList = map (lowerStr . unpackAtom)
        lowerStr         = map toLower




--setDelimiter :: Env -> Table -> String -> IOThrowsError WVal
--setDelimiter env table delim =
--    doTableWrite env table (\e t -> t {delimiter = delim})

setLabels :: Env -> Table -> [WVal] -> IOThrowsError WVal
setLabels env table labels = do
  unWrappedTable <- liftIO $ readIORef table

  checkLengthLabels unWrappedTable labels
  checkAllAtoms labels

  let labelsStrList = atomList2StrList labels

  checkAllUnique labelsStrList labels

  --let modTable = formatHelp unWrappedTable res
  let modTable = unWrappedTable { labels = labelsStrList }
  liftIO $ writeIORef table modTable
  return Unit

  where formatHelp tab formatList = tab { format = formatList  }
        atomList2StrList = map unpackAtom



--should in principle be hidden from evaluation.hs
dropColumn :: Env -> Table -> Integer -> IOThrowsError WVal
dropColumn env table index = do

  (dataTable, formats, labels) <- getTableStuff table

  let modifiedRows = map (removeAtIndex index) dataTable
  let modifiedFormat = removeAtIndex index formats
  let modifiedLabels = removeAtIndex index labels

  doTableWrite env table (\e t -> t {
    rows = modifiedRows,
    format = modifiedFormat,
    labels = modifiedLabels
  })

  return Unit

getTableStuff :: Table -> IOThrowsError ([[WVal]], [WType], [String])
getTableStuff table = liftIO $ fmap (\t -> (rows t, format t, labels t)) $ readIORef table


--helper function to remoe a single element from a list
-- NOTE: must be called with a valid index otherwise bad shit will happen
removeAtIndex :: Integer -> [a] -> [a]
removeAtIndex n l = (fst splitList) ++ (drop 1 $ snd splitList)
  where idx = fromIntegral n
        splitList = splitAt idx l

dropColumnIndex :: Env -> Table -> Integer -> IOThrowsError WVal
dropColumnIndex env table index = do
  checkIndex table index
  dropColumn env table index

dropColumnLabel :: Env -> Table -> String -> IOThrowsError WVal
dropColumnLabel env table label = do
  index <- fmap unpackInteger $ getLabelIndex table label
  dropColumn env table index



applyRowFunc :: Env -> Table -> WVal -> [WVal] -> IOThrowsError WVal
applyRowFunc env table f row = do
  if Top `elem` row then return Top else eval env table (List (f:row))

updateAtIndex :: Integer -> a -> [a] -> [a]
updateAtIndex n a l = (fst splitList) ++ (a:(drop 1 $ snd splitList))
  where idx = fromIntegral n
        splitList = splitAt idx l

getAt :: [Integer] -> [a] -> [a]
getAt ns l = map ((l !!) . fromIntegral) ns

transformColumns :: Env -> Table -> [Integer] -> Integer -> WVal -> IOThrowsError WVal
transformColumns env table indices destIndex f = do
  (dataTable, formats, _) <- getTableStuff table

  newColumnValues <- mapM ((applyRowFunc env table f) . (getAt indices)) dataTable
  let modifiedRows = map (uncurry $ updateAtIndex destIndex) $ zip newColumnValues dataTable

  let newType = getType $ head newColumnValues
  let modifiedFormat = updateAtIndex destIndex newType formats

  doTableWrite env table (\e t -> t {
    rows = modifiedRows,
    format = modifiedFormat
  })

  return Unit

applyBinaryRowFunc :: Env -> Table -> WVal -> Integer -> WVal -> [WVal] -> IOThrowsError WVal
applyBinaryRowFunc env table f index acc row = do
  let arg = row !! (fromIntegral index)
  if arg == Top
    then return acc
    else eval env table (List [f, acc, arg])

transformColumnIndex :: Env -> Table -> Integer -> WVal -> IOThrowsError WVal
transformColumnIndex env table index f = do
  checkIndex table index
  transformColumns env table [index] index f



transformColumnLabel :: Env -> Table -> String -> WVal -> IOThrowsError WVal
transformColumnLabel env table label f = do
  index <- fmap unpackInteger $ getLabelIndex table label
  transformColumns env table [index] index f


isLambda :: WVal -> Bool
isLambda (List (Atom "lambda" : List params : body)) = True
isLambda _ = False

grabIndices :: Table -> [WVal] -> IOThrowsError [Integer]
grabIndices table ((Integral i):xs) = do
  rest <- grabIndices table xs
  return $ i : rest
grabIndices table ((Atom l):xs) = do
  i <- fmap unpackInteger $ getLabelIndex table l
  rest <- grabIndices table xs
  return $ i : rest
grabIndices table [] = return []


transformColumnsList :: Env -> Table -> [WVal] -> IOThrowsError WVal
transformColumnsList env table list = do
  let (cols, rest) = break isLambda list
  indices <- grabIndices table cols

  if length rest /= 2
    then throwError $ NumArgs 2 rest
    else return Unit

  let f = head rest
  destCols <- grabIndices table (tail rest)
  let destCol = head destCols
  transformColumns env table indices destCol f


foldColumn :: Env -> Table -> Integer -> WVal -> WVal -> IOThrowsError WVal
foldColumn env table index f z = do
  (dataTable, formats, _) <- getTableStuff table

  value <- foldM (applyBinaryRowFunc env table f index) z dataTable

  return value

foldColumnIndex :: Env -> Table -> Integer -> WVal -> WVal -> IOThrowsError WVal
foldColumnIndex env table index f z = do
  checkIndex table index
  foldColumn env table index f z

foldColumnLabel :: Env -> Table -> String -> WVal -> WVal -> IOThrowsError WVal
foldColumnLabel env table label f z = do
  index <- fmap unpackInteger $ getLabelIndex table label
  foldColumn env table index f z

checkIndex :: Table -> Integer -> IOThrowsError WVal
checkIndex table index = do
  formatLength <- liftIO $ fmap (toInteger . length . format) $ readIORef table

  if index < 0 || index >= formatLength
    then throwError $ DataOperationIndex index formatLength
    else return Unit

{-- might be useful later but for now leave this out
getTableLength :: Table -> IO Integer
getTableLength table = do
  labelList <- fmap labels $ readIORef table
  return $ toInteger $ length labelList
--}


dropIncomplete :: Env -> Table -> IOThrowsError WVal
dropIncomplete env table = do
  (dataTable, _, _) <- getTableStuff table

  let modifiedRows = filter (not . (Top `elem`)) dataTable

  doTableWrite env table (\e t -> t {
    rows = modifiedRows
  })

  return Unit

--returns the index at which a label occurs or throws an error otherwise
getLabelIndex :: Table -> String -> IOThrowsError WVal
getLabelIndex table label = do
  unwrappedTable <- liftIO $ readIORef table
  let labelList = labels unwrappedTable

  let maybeIndex = fmap (Integral . toInteger) $ elemIndex label labelList

  if isNothing maybeIndex
    then throwError $ LabelDoesNotExist label
    else return $ fromJust maybeIndex


-- Helper function
doTableWrite :: Env -> Table -> (Env -> Table' -> Table') -> IOThrowsError WVal
doTableWrite env table f = do
    unwrappedTable <- liftIO $ readIORef table
    let t = f env unwrappedTable
    liftIO $ writeIORef table t
    return Unit


parseFile :: Env -> Table -> IOThrowsError WVal
parseFile env table = do

  args <- liftIO $ (readIORef env >>= readIORef . fromJust . lookup "args")

  let argList = unpackList args

  case length argList of 1 -> inFileHelp env table
                         2 -> inFileHelp env table >> outFileHelp (unpackString $ argList !! 1) env table
                         _ -> throwError CommandLineArgs

  --let infile  = unpackString $ argList !! 0
  --let outfile = unpackString $ argList !! 1

  --checkFileType infile  --check that the input file is a supported file type
  --checkFileType outfile --check that the output file is a supported file type

  --let outFileType = fromJust $ getFileType outfile

  --let infileParser = fromJust $ lookup (fromJust $ getFileType infile) fileParsers

  --doTableWrite env table (\e t -> t {outFileType = outFileType})
  --doTableWrite env table (\e t -> t {outFileName = outfile})

  --infileParser table infile

  --return Unit

outFileHelp outFile env table = do
                      --let outfile = unpackString $ argList !! outIdx
                      let outFileType = fromJust $ getFileType outFile
                      checkFileType outFile --check that the output file is a supported file type
                      doTableWrite env table (\e t -> t {outFileType = outFileType})
                      doTableWrite env table (\e t -> t {outFileName = outFile})
                      return Unit


inFileHelp env table = do
  args <- liftIO $ (readIORef env >>= readIORef . fromJust . lookup "args")
  let argList = unpackList args
  let infile = unpackString $ argList !! 0
  checkFileType infile
  let infileParser = fromJust $ lookup (fromJust $ getFileType infile) fileParsers
  infileParser table infile

setHasHeader :: Env -> Table -> IOThrowsError WVal
setHasHeader env table = do
  rows <- liftIO $ fmap rows $ readIORef table

  if null rows
    then throwError $ TableOperationOrder "hasHeader" "formatTable"
    else return Unit

  doTableWrite env table (\e t -> t {hasHeader = True})

  return Unit

parseFileRepl :: Env -> Table -> IOThrowsError WVal
parseFileRepl env table = do
  args <- liftIO $ (readIORef env >>= readIORef . fromJust . lookup "args")
  let argList = unpackList args

  return Unit

checkFileType :: String -> IOThrowsError WVal
checkFileType filename = if isNothing $ getFileType filename
                            then throwError $ UnsupportedFileType filename (map snd fileExtensions)
                            else return Unit

argLengthCheck :: [WVal] -> Int-> IOThrowsError WVal
argLengthCheck args n = if length args /= n then throwError $ CommandLineArgs
                                          else return Unit

getFileType :: String -> Maybe FileType
getFileType filename = lookup (dropWhile (/='.') filename) fileExtensions


writeTable :: Table -> IO ()
writeTable table = do
  table' <- readIORef table
  let outType = outFileType table'
  let outName = outFileName table'
  let outWriter = fromJust $ lookup outType fileWriters
  outWriter table outName
