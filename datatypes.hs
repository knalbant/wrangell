module DataTypes where

import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces, labels)
import Data.Maybe
import System.IO

import Data.IORef

data WType = TAtom
    | TList [WType]
    | TString
    | TBool
    | TIntegral
    | TFloat
    | TTop deriving (Show, Eq)  -- Everythign is a subtype of Top

data WVal = Atom String
          | List [WVal]
          | String String
          | Bool Bool
          | Integral Integer
          | Float Double
          | BuiltIn ([WVal] -> ThrowsError WVal)
          | Seq [WVal]
          | Func { params :: [String], body :: [WVal], closure :: Env }
          | IOFunc ([WVal] -> IOThrowsError WVal)
          | Top -- Everythign is a subtype of Top
          | Unit

instance Eq WVal where
  Atom a == Atom b              = a == b
  List a == List b              = a == b
  String a == String b          = a == b
  Bool a == Bool b              = a == b
  Integral a == Integral b      = a == b
  Float a == Float b            = a == b
  Seq a == Seq b                = a == b
  Func a b c ==  Func d e f     = (a == d) && (b == e) && (c == f)
  Top == Top                    = True
  Unit == Unit                  = True
  _ == _                        = False


data WError = Parser ParseError
            | NotFunction String String
            | UnboundVar String String
            | RedefineAttempt String String
            | TypeError String WVal
            | NumArgs Integer [WVal]
            | FormatSpec String [WVal]
            | DelimFormat [WVal]
            | NotImplemented String
            | UnsupportedFileType String [FileType]
            | CommandLineArgs
            | FormatNotDefined
            | TableOperation String String [WVal] --error when calling table transformation function
            | LabelDoesNotExist String --for calling
            | DataOperationIndex Integer Integer
            | CSVParseError String

data FileType = CSV
              | TSV
              | Text
              | File --placeholder, when initializing the emptytable need to have a value for all fields
              deriving (Show, Eq)

type Env = IORef [(String, IORef WVal)]

type IOThrowsError = ExceptT WError IO

type ThrowsError = Either WError

type FuncDef = (String, [WType])

data Table' = Table' { rows :: [[WVal]], format :: [WType],
                       labels :: [String],
                       outFileType :: FileType, outFileName :: String}

type Table = IORef Table'


instance Show WError where show = showError

instance Show WVal where show = showVal

nullEnv :: IO Env
nullEnv = newIORef []

--creates a new empty table context
emptyTable :: IO Table
emptyTable = newIORef Table' {
  rows   = [[]],
  format = [],
  labels = [],
  outFileType = File,
  outFileName = ""
}


makeFunc :: Env -> [WVal] -> [WVal] -> IOThrowsError WVal
makeFunc env params body = return $ Func (map show params) body env

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

--checks whether a variable is bound or not
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var

getVar :: Env -> String -> IOThrowsError WVal
getVar envRef var =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "Attempting to get an unbound variable" var)
           (liftIO . readIORef)
           (lookup var env)


--sets a value if its already been defined and returns the value it was set to for convenience
setVar :: Env -> String -> WVal -> IOThrowsError WVal
setVar envRef var value =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "Setting an unbound variable" var)
           (liftIO . ( `writeIORef` value)) --the first value to writeIORef should be an IORef
           (lookup var env)
     return value


defineVar :: Env -> String -> WVal -> IOThrowsError WVal
defineVar envRef var value =
  do env <- liftIO $ readIORef envRef
     alreadyBound <- liftIO $ isBound envRef var
     if alreadyBound then
       --setVar envRef var value >> return value
       throwError $ RedefineAttempt "Attempting to re-define a previously defined variable" var
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

bindVars :: Env -> [(String, WVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var,ref)


getType :: WVal -> WType
getType (Atom _) = TAtom
getType (List l) = TList $ map getType l
getType (String _) = TString
getType (Bool _) = TBool
getType (Integral _) = TIntegral
getType (Float _) = TFloat


unwordsList :: [WVal] -> String
unwordsList = unwords . map showVal

--returns the string representations of our WVal datatype
showVal :: WVal -> String
showVal (Atom atom)     = atom
showVal (String str)    = str
showVal (Bool True)     = "true"
showVal (Bool False)    = "false"
showVal (Integral n)    = show n
showVal (Float f)       = show f
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (BuiltIn _)     = "<BuiltIn function>"
showVal (Func args body env)
         = "(lambda (" ++ unwords (map show args) ++ ") ...)"
showVal (IOFunc _)      = "<IOFunc>"
showVal Top             = "<Top>"
showVal Unit            = ""

commandLineErrString = "Usage: ./wrangel [wrangell script] [infile] [outfile]"

showError :: WError -> String
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (NotFunction message func) = message ++ " " ++ func
showError (UnboundVar message varId) = message ++ ": " ++ varId
showError (RedefineAttempt message varId) = message ++ ": " ++ varId
showError (TypeError expected found) =  "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found

showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ " args; found values " ++ unwordsList found

showError (FormatSpec message values) = message ++ unwordsList values
showError (DelimFormat found) =
  "Delimiter specification should be (delimiter \"delims\") got: " ++ unwordsList found

--showError (CommandLineArgs) = "./wrangell [wrangell file] [input file] [outputfile]"
showError CommandLineArgs = "Usage: ./wrangel [wrangell script] [infile] [outfile]"

showError (UnsupportedFileType extension fileTypes)
  = "Unsupported filetype got file: " ++ extension
      ++ " currently supporting: " ++ unwords  (map show fileTypes)
showError (CSVParseError err) = "Error parsing CSV: " ++ err

showError FormatNotDefined =
  "Format of the input file must be defined before doing any transformations on the data"

showError (NotImplemented message) = message

showError (TableOperation funcName expectedArgs foundArgs) =
  unwords [funcName, " expects: ", expectedArgs, " got: ", unwords $ map show foundArgs]

showError (LabelDoesNotExist label) =
  "The label: " ++ label ++ " does not exist in the label list."

showError (DataOperationIndex index maxIndex) =
  "Index to data operations must be between 0 and maximum possible range got index: " ++
  show index ++ " where maximum possible index is numCols: " ++ show (maxIndex - 1)

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unpackAtom :: WVal -> String
unpackAtom (Atom str) = str

unpackInteger :: WVal -> Integer
unpackInteger (Integral n) = n

unpackFloat :: WVal -> Double
unpackFloat (Float f) = f

unpackBool :: WVal -> Bool
unpackBool (Bool b) = b

unpackString :: WVal -> String
unpackString (String s) = s

unpackList :: WVal -> [WVal]
unpackList (List l) = l
