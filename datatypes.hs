module DataTypes where

import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)


import Data.IORef

data WType = TAtom
    | TList [WType]
    | TString
    | TBool
    | TIntegral
    | TFloat deriving (Show, Eq)

data WVal = Atom String
          | List [WVal]
          | String String
          | Bool Bool
          | Integral Integer
          | Float Double
          | BuiltIn ([LispVal] -> ThrowsError LispVal)
          | Func { params :: [String], body :: [WVal], closure :: Env }

data WError = Parser ParseError
            | NotFunction String String
            | UnboundVar String String
            | RedefineAttempt String String
            | TypeError String WVal
            | NumArgs Integer [WVal]

type Env = IORef [(String, IORef WVal)]

type IOThrowsError = ExceptT WError IO

type ThrowsError = Either WError

type FuncDef = (String, [WType])


instance Show WError where show = showError

instance Show WVal where show = showVal

nullEnv :: IO Env
nullEnv = newIORef []


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

--checks whether a variable is bound or not
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

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
showVal (Bool True)     = "#t"
showVal (Bool False)    = "#f"
showVal (Integral n)    = show n
showVal (Float f)       = show f
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (BuiltIn _)     = "<BuiltIn function>"
showVal (Func {params = params,
               body   = body,
               env    = env}) = "(lambda (" ++ unwords (map show args) ++ ") ...)"


showError :: WError -> String
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (NotFunction message func) = message ++ " " ++ func
showError (UnboundVar message varId) = message ++ ": " ++ varId
showError (RedefineAttempt message varId) = message ++ ": " ++ varId
showError (TypeError expected found) =  "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found

showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ " args; found values " ++ unwordsList found


trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
