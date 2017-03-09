module DataTypes where


data WVal = Atom String
          | List [WVal]
          | String String
          | Bool Bool
          | Integral Integer
          | Float Double


instance Show WVal where show = showVal


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
