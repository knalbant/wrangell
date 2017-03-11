module Evaluation (eval) where
import DataTypes

eval :: WVal -> WVal
eval val@(String _) = val
eval val@(Integral _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [WVal] -> WVal
apply func args = maybe (Bool False) ($ args) $ lookup (func, types) funcTable
    where types = map getType args


type FuncDef = (String, [WType])
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
    (("/", [TFloat, TFloat]), floatBinaryOp (/))
    ]


integerUnaryOp :: (Integer -> Integer) -> [WVal] -> WVal
integerUnaryOp op = Integral . op . unpackInteger . head

integerBinaryOp :: (Integer -> Integer -> Integer) -> [WVal] -> WVal
integerBinaryOp op params = Integral $ foldl1 op $ map unpackInteger params

floatUnaryOp :: (Double -> Double) -> [WVal] -> WVal
floatUnaryOp op = Float . op . unpackFloat . head

floatBinaryOp :: (Double -> Double -> Double) -> [WVal] -> WVal
floatBinaryOp op params = Float $ foldl1 op $ map unpackFloat params



unpackInteger :: WVal -> Integer
unpackInteger (Integral n) = n

unpackFloat :: WVal -> Double
unpackFloat (Float f) = f
