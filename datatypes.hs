module DataTypes where


 data WVal = Atom String
           | List [WVal]
           | DottedList [WVal] WVal
           | String String
           | Bool Bool
           | Integral Integer
           | Float Double

--instance show WVal where show = showVal
