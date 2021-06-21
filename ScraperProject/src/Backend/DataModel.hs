module Backend.DataModel where

import Database.MySQL.Base

data SQLRecord = SQLRecord {
    id :: Int,
    title :: String,
    price :: String,
    description :: String,
    priceFrom :: String,
    priceTo :: String,
    flatFrom :: String,
    flatTo :: String,
    roomsFrom :: String,
    roomsTo :: String,
    isRead :: Int,
    isInteresting :: Int
}

mappingValueToString :: MySQLValue -> String
mappingValueToString x = show x