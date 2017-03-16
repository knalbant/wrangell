module Functions (funcTable) where


import DataTypes
import Data.List
import Control.Monad.Except



checkLength :: (Integer -> Integer -> Bool) -> Integer -> [WVal] -> ThrowsError WVal
checkLength pred n values = if genericLength values `pred` n then return $ List values else throwError (NumArgs n values)

checkAllType :: WType -> String -> [WVal] -> ThrowsError WVal
checkAllType expectedType message vals =
  if all (==expectedType) (map getType vals)
  then return $ List vals
  else throwError $ TypeError message $ List vals


-- Math Arithmetic Function Types
integerUnaryOp :: (Integer -> Integer) -> [WVal] -> ThrowsError WVal
--integerUnaryOp op = Integral . op . unpackInteger . head
integerUnaryOp op params =
  --check the precondidtions
  checkLength (==) 1 params >>=
    checkAllType TIntegral "one integral" . unpackList >>=
      return . Integral . op . unpackInteger . head . unpackList


integerBinaryOp :: (Integer -> Integer -> Integer) -> [WVal] -> ThrowsError WVal
integerBinaryOp op params =
  checkLength (>=) 2 params >>=
    checkAllType TIntegral "2 or more integers" . unpackList
      >>= return . Integral . foldl1 op . map unpackInteger . unpackList


floatUnaryOp :: (Double -> Double) -> [WVal] -> ThrowsError WVal
floatUnaryOp op params =
  checkLength (==) 1 params >>=
    checkAllType TFloat "1 integer" . unpackList >>=
      return . Float . op . unpackFloat . head . unpackList

floatBinaryOp :: (Double -> Double -> Double) -> [WVal] -> ThrowsError WVal
floatBinaryOp op params =
  checkLength (>=) 2 params >>=
    checkAllType TFloat "2 or more integers" . unpackList >>=
      return . Float . foldl1 op . map unpackFloat . unpackList


-- Boolean Arithmetic Function Types
boolUnaryOp :: (Bool -> Bool) -> [WVal] -> ThrowsError WVal
boolUnaryOp op params =
  checkLength (==) 1 params >>=
    checkAllType TBool "1 boolean" . unpackList >>=
      return . Bool . op . unpackBool . head . unpackList

boolBinaryOp :: (Bool -> Bool -> Bool) -> [WVal] -> ThrowsError WVal
boolBinaryOp op params =
  checkLength (>=) 2 params >>=
    checkAllType TBool "2 or more bools" . unpackList >>=
      return . Bool . foldl1 op . map unpackBool . unpackList


-- Comparision Function Types
integerBinaryBoolOp :: (Integer -> Integer -> Bool) -> [WVal] -> ThrowsError WVal
integerBinaryBoolOp op params = do --Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    params <- checkLength (>=) 2 params >>= checkAllType TIntegral "2 or more integers" . unpackList
    return $ Bool $ and $ zipWith op unpackedParams $ tail unpackedParams
    where unpackedParams = map unpackInteger params

floatBinaryBoolOp :: (Double -> Double -> Bool) -> [WVal] -> ThrowsError WVal
floatBinaryBoolOp op params = do --Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    params <- checkLength (>=) 2 params >>= checkAllType TFloat "2 or more floats" . unpackList
    return $ Bool $ and $ zipWith op unpackedParams $ tail unpackedParams
    where unpackedParams = map unpackFloat params

stringBinaryBoolOp :: (String -> String -> Bool) -> [WVal] -> ThrowsError WVal
stringBinaryBoolOp op params = do --Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    params <- checkLength (>=) 2 params >>= checkAllType TBool "2 or more booleans" . unpackList
    return $ Bool $ and $ zipWith op unpackedParams $ tail unpackedParams
    where unpackedParams = map unpackString params


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
    ("not", boolUnaryOp not)
    ]

    --(("if", if'),
    --(("if", if'),
    --(("if", if'),
    --(("if", if')]



--if' :: [WVal] -> WVal
--if' [condition, t, f] = if unpackBool condition then t else f
