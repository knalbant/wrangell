module Functions (funcTable) where
import DataTypes






-- Math Arithmetic Function Types
integerUnaryOp :: (Integer -> Integer) -> [WVal] -> WVal
integerUnaryOp op = Integral . op . unpackInteger . head

integerBinaryOp :: (Integer -> Integer -> Integer) -> [WVal] -> WVal
integerBinaryOp op params = Integral $ foldl1 op $ map unpackInteger params

floatUnaryOp :: (Double -> Double) -> [WVal] -> WVal
floatUnaryOp op = Float . op . unpackFloat . head

floatBinaryOp :: (Double -> Double -> Double) -> [WVal] -> WVal
floatBinaryOp op params = Float $ foldl1 op $ map unpackFloat params


-- Boolean Arithmetic Function Types
boolUnaryOp :: (Bool -> Bool) -> [WVal] -> WVal
boolUnaryOp op = Bool . op . unpackBool . head

boolBinaryOp :: (Bool -> Bool -> Bool) -> [WVal] -> WVal
boolBinaryOp op params = Bool $ foldl1 op $ map unpackBool params


-- Comparision Function Types
integerBinaryBoolOp :: (Integer -> Integer -> Bool) -> [WVal] -> WVal
integerBinaryBoolOp op params = Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    where unpackedParams = map unpackInteger params

floatBinaryBoolOp :: (Double -> Double -> Bool) -> [WVal] -> WVal
floatBinaryBoolOp op params = Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    where unpackedParams = map unpackFloat params

stringBinaryBoolOp :: (String -> String -> Bool) -> [WVal] -> WVal
stringBinaryBoolOp op params = Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    where unpackedParams = map unpackString params

-- List Functions
head' :: [WVal] -> WVal
head' [List (x : xs)] = x


unpackInteger :: WVal -> Integer
unpackInteger (Integral n) = n

unpackFloat :: WVal -> Double
unpackFloat (Float f) = f

unpackBool :: WVal -> Bool
unpackBool (Bool b) = b

unpackString :: WVal -> String
unpackString (String s) = s

funcTable :: [(FuncDef, [WVal] -> WVal)]
funcTable =
    [
    (("+", [TIntegral, TIntegral]), integerBinaryOp (+)),
    (("+", [TFloat, TFloat]), floatBinaryOp (+)),
    (("-", [TIntegral, TIntegral]), integerBinaryOp (-)),
    (("-", [TFloat, TFloat]), floatBinaryOp (-)),
    (("-", [TIntegral]), integerUnaryOp (0-)),
    (("-", [TFloat]), floatUnaryOp (0-)),
    (("*", [TIntegral, TIntegral]), integerBinaryOp (*)),
    (("*", [TFloat, TFloat]), floatBinaryOp (*)),
    (("/", [TIntegral, TIntegral]), integerBinaryOp quot),
    (("/", [TFloat, TFloat]), floatBinaryOp (/)),
    (("=", [TIntegral, TIntegral]), integerBinaryBoolOp (==)),
    (("<", [TIntegral, TIntegral]), integerBinaryBoolOp (<)),
    ((">", [TIntegral, TIntegral]), integerBinaryBoolOp (>)),
    (("/=", [TIntegral, TIntegral]), integerBinaryBoolOp (/=)),
    (("<=", [TIntegral, TIntegral]), integerBinaryBoolOp (<=)),
    ((">=", [TIntegral, TIntegral]), integerBinaryBoolOp (>=)),
    (("=", [TFloat, TFloat]), floatBinaryBoolOp (==)),
    (("<", [TFloat, TFloat]), floatBinaryBoolOp (<)),
    ((">", [TFloat, TFloat]), floatBinaryBoolOp (>)),
    (("/=", [TFloat, TFloat]), floatBinaryBoolOp (/=)),
    (("<=", [TFloat, TFloat]), floatBinaryBoolOp (<=)),
    ((">=", [TFloat, TFloat]), floatBinaryBoolOp (>=)),
    (("=", [TString, TString]), stringBinaryBoolOp (==)),
    (("<", [TString, TString]), stringBinaryBoolOp (<)),
    ((">", [TString, TString]), stringBinaryBoolOp (>)),
    (("/=", [TString, TString]), stringBinaryBoolOp (/=)),
    (("<=", [TString, TString]), stringBinaryBoolOp (<=)),
    ((">=", [TString, TString]), stringBinaryBoolOp (>=)),
    (("&&", [TBool, TBool]), boolBinaryOp (&&)),
    (("||", [TBool, TBool]), boolBinaryOp (||)),
    (("not", [TBool]), boolUnaryOp not),
    (("if", [TBool, TIntegral, TIntegral]), if'),
    (("if", [TBool, TFloat, TFloat]), if'),
    (("if", [TBool, TBool, TBool]), if'),
    (("if", [TBool, TString, TString]), if')]
    --(("head", [TList [TIntegral,TIntegral,TIntegral]]), head' )


   
if' :: [WVal] -> WVal
if' [condition, t, f] = if unpackBool condition then t else f
