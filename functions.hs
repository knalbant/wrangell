module Functions (funcTable) where


import DataTypes
import Data.List
import Control.Monad.Except


{--
I've now slightly changed the way that the checking of fucntion parameters happens now

Instead of having the constraints of the parameters hardcoded in the function table
  I've now shunted the type checking to the bodies of the functions themselves
  Everything that was previously expressible in prior setup is expressible here
  but now the constraints that can be represented are much richer
--}


--given a predicate which compares integers do some sort of check on the sizes of the lists
--  we are passed in
checkLength :: (Integer -> Integer -> Bool) -> Integer -> [WVal] -> ThrowsError WVal
checkLength pred n values = if genericLength values `pred` n then return $ List values else throwError (NumArgs n values)

--checks whether all the types in a list match
checkAllTypes :: WType -> String -> [WVal] -> ThrowsError WVal
checkAllTypes expectedType message vals =
  if all (==expectedType) (map getType vals)
  then return $ List vals
  else throwError $ TypeError message $ List vals

checkType :: WType -> WVal -> ThrowsError WVal
checkType wType val = if getType val == wType
                      then return val else throwError $ TypeError "bool" val

-- Math Arithmetic Function Types
integerUnaryOp :: (Integer -> Integer) -> [WVal] -> ThrowsError WVal
--integerUnaryOp op = Integral . op . unpackInteger . head
integerUnaryOp op params =
  --check the precondidtions
  checkLength (==) 1 params >>=
    checkAllTypes TIntegral "one integral" . unpackList >>=
      return . Integral . op . unpackInteger . head . unpackList


integerBinaryOp :: (Integer -> Integer -> Integer) -> [WVal] -> ThrowsError WVal
integerBinaryOp op params =
  checkLength (>=) 2 params >>=
    checkAllTypes TIntegral "2 or more integers" . unpackList
      >>= return . Integral . foldl1 op . map unpackInteger . unpackList


floatUnaryOp :: (Double -> Double) -> [WVal] -> ThrowsError WVal
floatUnaryOp op params =
  checkLength (==) 1 params >>=
    checkAllTypes TFloat "1 integer" . unpackList >>=
      return . Float . op . unpackFloat . head . unpackList

floatBinaryOp :: (Double -> Double -> Double) -> [WVal] -> ThrowsError WVal
floatBinaryOp op params =
  checkLength (>=) 2 params >>=
    checkAllTypes TFloat "2 or more integers" . unpackList >>=
      return . Float . foldl1 op . map unpackFloat . unpackList


-- Boolean Arithmetic Function Types
boolUnaryOp :: (Bool -> Bool) -> [WVal] -> ThrowsError WVal
boolUnaryOp op params =
  checkLength (==) 1 params >>=
    checkAllTypes TBool "1 boolean" . unpackList >>=
      return . Bool . op . unpackBool . head . unpackList

boolBinaryOp :: (Bool -> Bool -> Bool) -> [WVal] -> ThrowsError WVal
boolBinaryOp op params =
  checkLength (>=) 2 params >>=
    checkAllTypes TBool "2 or more bools" . unpackList >>=
      return . Bool . foldl1 op . map unpackBool . unpackList


-- Comparision Function Types
integerBinaryBoolOp :: (Integer -> Integer -> Bool) -> [WVal] -> ThrowsError WVal
integerBinaryBoolOp op params = do --Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    params <- checkLength (>=) 2 params >>= checkAllTypes TIntegral "2 or more integers" . unpackList
    return $ Bool $ and $ zipWith op unpackedParams $ tail unpackedParams
    where unpackedParams = map unpackInteger params

floatBinaryBoolOp :: (Double -> Double -> Bool) -> [WVal] -> ThrowsError WVal
floatBinaryBoolOp op params = do --Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    params <- checkLength (>=) 2 params >>= checkAllTypes TFloat "2 or more floats" . unpackList
    return $ Bool $ and $ zipWith op unpackedParams $ tail unpackedParams
    where unpackedParams = map unpackFloat params

stringBinaryBoolOp :: (String -> String -> Bool) -> [WVal] -> ThrowsError WVal
stringBinaryBoolOp op params = do --Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    params <- checkLength (>=) 2 params >>= checkAllTypes TBool "2 or more booleans" . unpackList
    return $ Bool $ and $ zipWith op unpackedParams $ tail unpackedParams
    where unpackedParams = map unpackString params

car :: [WVal] -> ThrowsError WVal
car [List (x : _)] = return x
car [badArg]       = throwError $ TypeError "pair" badArg
car badArgList     = throwError $ NumArgs 1 badArgList

cdr :: [WVal] -> ThrowsError WVal
cdr [List (_ : xs)] = return $ List xs
cdr [badArg]       = throwError $ TypeError "pair" badArg
cdr badArgList     = throwError $ NumArgs 1 badArgList


if' :: [WVal] -> ThrowsError WVal
if' ifComps = do
  --error checking
  checkLength (==) 3 ifComps
  checkType TBool $ head ifComps

  --checks that the types of the consequents match
  if getType t == getType f
  then return $ List $ tail ifComps
  else throwError $ TypeError "expected types to match, found" $ List $ tail ifComps

  --does the actual if computation
  if unpackBool cond then return t else return f

  where cond = head ifComps
        t    = ifComps !! 1
        f    = ifComps !! 2

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

funcTable :: [(String, [WVal] -> ThrowsError WVal)]
funcTable =
    [
    ("-", integerUnaryOp (0-)),
    ("+", integerBinaryOp (+)),
    ("+", floatBinaryOp (+)),
    ("-", integerBinaryOp (-)),
    ("-", floatBinaryOp (-)),
    ("-", floatUnaryOp (0-)),
    ("*", integerBinaryOp (*)),
    ("*", floatBinaryOp (*)),
    ("/", integerBinaryOp quot),
    ("/", floatBinaryOp (/)),
    ("=", integerBinaryBoolOp (==)),
    ("<", integerBinaryBoolOp (<)),
    (">", integerBinaryBoolOp (>)),
    ("/=", integerBinaryBoolOp (/=)),
    ("<=", integerBinaryBoolOp (<=)),
    (">=", integerBinaryBoolOp (>=)),
    ("=", floatBinaryBoolOp (==)),
    ("<", floatBinaryBoolOp (<)),
    (">", floatBinaryBoolOp (>)),
    ("/=", floatBinaryBoolOp (/=)),
    ("<=", floatBinaryBoolOp (<=)),
    (">=", floatBinaryBoolOp (>=)),
    ("=", stringBinaryBoolOp (==)),
    ("<", stringBinaryBoolOp (<)),
    (">", stringBinaryBoolOp (>)),
    ("/=", stringBinaryBoolOp (/=)),
    ("<=", stringBinaryBoolOp (<=)),
    (">=", stringBinaryBoolOp (>=)),
    ("&&", boolBinaryOp (&&)),
    ("||", boolBinaryOp (||)),
    ("not", boolUnaryOp not),
    ("if", if'),
    ("car", car),
    ("cdr", cdr)
    ]
