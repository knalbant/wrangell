module Evaluation where
import DataTypes

eval :: Env -> Table -> WVal -> IOThrowsError WVal