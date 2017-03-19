module DataOperations where
import DataTypes
import Control.Monad.Except
import ArgParsing
import Data.IORef
import Data.Char
import Data.Maybe

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

checkLengthFormats :: Table' -> [WVal] -> IOThrowsError WVal
checkLengthFormats table formats
  | null formats =
    throwError $
      FormatSpec "Formats should have at least one element found: " formats

  | not (null (format table)) &&
      length formats /= length (labels table)
    =
    throwError $
      FormatSpec "Formats should have the same length as labels: " formats

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
      FormatSpec "Labels should have the same length as formats: " labels

  | otherwise = return Unit

checkAllAtoms :: [WVal] -> IOThrowsError WVal
checkAllAtoms formats = if all (==TAtom) $ map getType formats
                        then return Unit
                        else throwError $ FormatSpec "Formats should be a list of all atoms found: " formats

checkAllUnique :: [String] -> [WVal] -> IOThrowsError WVal
checkAllUnique labels wlabels = if allUnique labels
                        then return $ Integral 0 --basically just a dummy value
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




setDelimiter :: Env -> Table -> String -> IOThrowsError WVal
setDelimiter env table delim =
    doTableWrite env table (\e t -> (t {delimiter = delim}, Unit))

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


dropColumn :: Env -> Table -> WVal -> IOThrowsError WVal
dropColumn env table (Integral index) = return Unit
dropColumn env table (String label) = return Unit
dropColumn env table val = throwError $ TypeError "Invalid type for dropColumn" val



-- Helper function
doTableWrite :: Env -> Table -> (Env -> Table' -> (Table', WVal)) -> IOThrowsError WVal
doTableWrite env table f = do
    unwrappedTable <- liftIO $ readIORef table
    let (t, ret) = f env unwrappedTable
    liftIO $ writeIORef table t
    return ret
