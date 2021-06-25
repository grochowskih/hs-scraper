module Backend.DataModel where

import Database.MySQL.Base

-- |Funkcja, która na podstawie wartośc z MySQL tworzy String.
mappingValueToString :: MySQLValue -> String
mappingValueToString (MySQLText x) = show x
mappingValueToString (MySQLInt32 x) = show x
mappingValueToString (MySQLInt8 x) = show x
mappingValueToString x = show x

-- |Funkcja, która mapuje listy list MySQLValue (lista outputów z bazy danych) na listę list String.
mappingValuesToList :: [[MySQLValue]] -> [[String]]
mappingValuesToList args = map (map mappingValueToString) args