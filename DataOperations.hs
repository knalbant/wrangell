module DataOperations where
import DataTypes
import Control.Monad.Except
import ArgParsing
import Data.IORef
import Data.Char
import Data.Maybe

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
                      else return Unit

checkAllAtoms :: [WVal] -> IOThrowsError WVal
checkAllAtoms formats = if all (==TAtom) $ map getType formats
                        then return Unit
                        else throwError $ FormatSpec "Formats should be a list of all atoms found: " formats


formatTable :: Env -> Table -> [WVal] -> IOThrowsError WVal
formatTable env table formats = do

  --prechecks
  checkLength formats
  checkAllAtoms formats

  unWrappedTable <- liftIO $ readIORef table

  let formatStrList = atomList2StrList formats

  --returns Nothing if an invalid type specifier is found otherwise returns a Just [WType]
  let res = sequenceA $ map (flip lookup typeTable) formatStrList

  if isJust res
    then do
      let modTable = formatHelp unWrappedTable $ fromJust res
      liftIO $ writeIORef table modTable
      return $ Unit
    else
      throwError $
      FormatSpec ("Format specifiers are one of " ++
                  (unwords $ map fst typeTable) ++ " found: ") formats

        --returns a new table with format filled in
  where formatHelp tab formatList = tab { format = formatList  }
        --turns a list of atoms into a list of strings while also ignoring case
        atomList2StrList = map (lowerStr . unpackAtom)
        lowerStr         = map toLower




setDelimiter :: Env -> Table -> String -> IOThrowsError WVal
setDelimiter env table delim = do
    unwrappedTable <- liftIO $ readIORef table
    liftIO $ writeIORef table (unwrappedTable {delimiter = delim})
    return $ Unit

-- setDelimiter' :: Table' -> String {}

dropColumn :: Env -> Table -> WVal -> IOThrowsError WVal
dropColumn env table (Integral index) = return Unit
dropColumn env table (String label) = return Unit
dropColumn env table val = throwError $ TypeError "Invalid type for dropColumn" val
