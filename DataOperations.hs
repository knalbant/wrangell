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

checkLength :: [WVal] -> IOThrowsError WVal
checkLength formats = if length formats < 1
                      then throwError $ FormatSpec "Formats should have at least one element found: " formats
                      else return $ Integral 0
checkLengthLabels :: Table' -> [WVal] -> IOThrowsError WVal
checkLengthLabels table labels = 
    if length labels < 1
        then throwError $ FormatSpec "Labels should have at least one element found: " labels
    else if (length labels) /= (length $ format table)
        then throwError $ FormatSpec "Labels should have at least one element found: " labels
    else return $ Integral 0

checkAllAtoms :: [WVal] -> IOThrowsError WVal
checkAllAtoms formats = if all (==TAtom) $ map getType formats
                        then return $ Integral 0 --basically just a dummy value
                        else throwError $ FormatSpec "Formats should be a list of all atoms found: " formats

checkAllUnique :: [String] -> [WVal] -> IOThrowsError WVal
checkAllUnique labels wlabels = if allUnique labels
                        then return $ Integral 0 --basically just a dummy value
                        else throwError $ FormatSpec "Labels should be all unique: " wlabels


formatTable :: Env -> Table -> [WVal] -> IOThrowsError WVal
formatTable env table formats = do

  checkLength formats
  checkAllAtoms formats


  unWrappedTable <- liftIO $ readIORef table

  let formatStrList = atomList2StrList formats

  let res = sequenceA $ map (flip lookup typeTable) formatStrList

  --let modTable = formatHelp unWrappedTable res

  if isJust res
    then do
      let modTable = formatHelp unWrappedTable $ fromJust res
      liftIO $ writeIORef table modTable
      return $ Integral 0
    else
      throwError $
      FormatSpec ("Format specifiers are one of " ++
                  (unwords $ map fst typeTable) ++ " found: ") formats


  where formatHelp tab formatList = tab { format = formatList  }
        atomList2StrList = map (lowerStr . unpackAtom)
        lowerStr         = map toLower




setDelimiter :: Env -> Table -> String -> IOThrowsError WVal
setDelimiter env table delim = 
    doTableWrite env table (\e t -> (t {delimiter = delim}, Integral 0))

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
  return $ Integral 0

  where formatHelp tab formatList = tab { format = formatList  }
        atomList2StrList = map unpackAtom



dropColumn :: Env -> Table -> WVal -> IOThrowsError WVal
dropColumn env table (Integral index) = return $ Integral 0
dropColumn env table (String label) = return $ Integral 1
dropColumn env table val = throwError $ TypeError "Invalid type for dropColumn" val



-- Helper function
doTableWrite :: Env -> Table -> (Env -> Table' -> (Table', WVal)) -> IOThrowsError WVal
doTableWrite env table f = do
    unwrappedTable <- liftIO $ readIORef table
    let (t, ret) = f env unwrappedTable 
    liftIO $ writeIORef table t
    return $ ret



unpackAtomString :: WVal -> String
unpackAtomString (Atom s) = s