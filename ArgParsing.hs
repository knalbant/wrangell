module ArgParsing where
import DataTypes
import Control.Monad.Except

data ParsedArgs = ParsedArgs {
    inputFile :: String,
    outputFile :: String
}

getInputFile :: ParsedArgs -> IOThrowsError String
getInputFile p = return $ inputFile p



getParsedArgs :: Env -> IOThrowsError ParsedArgs
getParsedArgs env = getVar env "args" >>= unpackArgs


-- TODO: Change this to make this more flexible!!!
unpackArgs :: WVal -> IOThrowsError ParsedArgs
unpackArgs (List wargs) = return ParsedArgs { inputFile=args !! 0, outputFile=args !! 1 }
    where args = map unpackString wargs
