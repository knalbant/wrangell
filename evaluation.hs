module Evaluation (eval) where
import Control.Monad.Except
import Data.Maybe

import DataTypes
import Errors

eval :: WVal -> ThrowsError WVal
eval val@(String _) = return val
eval val@(Integral _) = return val
eval val@(Bool _) = return val
eval val@(Float _) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [WVal] -> ThrowsError WVal
apply func args = if isNothing foundFunc
    then throwError badFuncError
    else return ((fromJust foundFunc) args)
    where types = convertListOp $ map getType args
          badFuncError = NotFunction (func, types)
          foundFunc = lookup (func, types) funcTable


--a hacky fix to have wrangell work with lists of the same type
convertListOp :: [WType] -> [WType]
convertListOp typeList =
  if length typeList > 2 then
    if all (== (head typeList)) typeList then take 2 typeList else typeList
  else typeList




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
    (("&&", [TBool, TBool]), boolBinaryOp (&&)),
    (("||", [TBool, TBool]), boolBinaryOp (||)),
    (("not", [TBool]), boolUnaryOp not)
    ]


integerUnaryOp :: (Integer -> Integer) -> [WVal] -> WVal
integerUnaryOp op = Integral . op . unpackInteger . head

integerBinaryOp :: (Integer -> Integer -> Integer) -> [WVal] -> WVal
integerBinaryOp op params = Integral $ foldl1 op $ map unpackInteger params

floatUnaryOp :: (Double -> Double) -> [WVal] -> WVal
floatUnaryOp op = Float . op . unpackFloat . head

floatBinaryOp :: (Double -> Double -> Double) -> [WVal] -> WVal
floatBinaryOp op params = Float $ foldl1 op $ map unpackFloat params

boolUnaryOp :: (Bool -> Bool) -> [WVal] -> WVal
boolUnaryOp op = Bool . op . unpackBool . head

boolBinaryOp :: (Bool -> Bool -> Bool) -> [WVal] -> WVal
boolBinaryOp op params = Bool $ foldl1 op $ map unpackBool params

integerBinaryBoolOp :: (Integer -> Integer -> Bool) -> [WVal] -> WVal
integerBinaryBoolOp op params = Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    where unpackedParams = map unpackInteger params

floatBinaryBoolOp :: (Double -> Double -> Bool) -> [WVal] -> WVal
floatBinaryBoolOp op params = Bool $ foldl (\b (x, y) -> b && (op x y)) True $ zip unpackedParams (tail unpackedParams)
    where unpackedParams = map unpackFloat params

unpackInteger :: WVal -> Integer
unpackInteger (Integral n) = n

unpackFloat :: WVal -> Double
unpackFloat (Float f) = f

unpackBool :: WVal -> Bool
unpackBool (Bool b) = b
