module Backend.DataModel where

import Database.MySQL.Base

mappingValueToString :: MySQLValue -> String
mappingValueToString (MySQLText x) = show x
mappingValueToString (MySQLInt32 x) = show x
mappingValueToString (MySQLInt8 x) = show x
mappingValueToString x = show x

mappingValuesToList :: [[MySQLValue]] -> [[String]]
mappingValuesToList args = map (map mappingValueToString) args