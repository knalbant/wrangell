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
    (("/", [TFloat, TFloat]), floatBinaryOp (/))
    ]


integerUnaryOp :: (Integer -> Integer) -> [WVal] -> WVal
integerUnaryOp op = Integral . op . unpackInteger . head

integerBinaryOp :: (Integer -> Integer -> Integer) -> [WVal] -> WVal
integerBinaryOp op params = Integral $ foldl1 op $ map unpackInteger params
-- integerBinaryOp op params = if length params == 2
--     then mapM unpackInteger >>= return . Integral . foldl1 op
--     else throwError $ NumArgs 2 params

floatUnaryOp :: (Double -> Double) -> [WVal] -> WVal
floatUnaryOp op = Float . op . unpackFloat . head

floatBinaryOp :: (Double -> Double -> Double) -> [WVal] -> WVal
floatBinaryOp op params = Float $ foldl1 op $ map unpackFloat params



unpackInteger :: WVal -> Integer
unpackInteger (Integral n) = n

unpackFloat :: WVal -> Double
unpackFloat (Float f) = f
