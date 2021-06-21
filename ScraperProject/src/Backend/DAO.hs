{-# LANGUAGE OverloadedStrings #-}
module Backend.DAO where

import Database.MySQL.Base
import System.IO.Streams as Streams
import Data.Int
import Backend.Scraper
import Data.Maybe
import Data.Text

createConnection :: IO MySQLConn 
createConnection = do
    conn <- Database.MySQL.Base.connect
        defaultConnectInfo {ciHost = "sql11.freesqldatabase.com", ciPort = 3306, ciUser = "sql11417464", ciPassword = "XDMEcWeuRp", ciDatabase = "sql11417464"}
    putStrLn "Nawiazalem polaczenie"
    return conn

getOffers :: MySQLConn -> IO [[String]]
getOffers conn = do
    (defs, is) <- query_ conn "SELECT * FROM offers"
    --Streams.toList is
    return $ Prelude.map (Prelude.map show) [[MySQLInt32 1,MySQLText "Test",MySQLText "155",MySQLText "OpisTestowy",MySQLText "100",MySQLText "200",MySQLText "100",MySQLText "200",MySQLText "1",MySQLText "2",MySQLInt8 0,MySQLInt8 1]]

getInterestingOffers :: MySQLConn -> IO ()
getInterestingOffers conn = do
    (defs, is) <- query_ conn "SELECT * FROM offers WHERE isInteresting = 1 "
    putStrLn "Wykonalem selecta interesujacych"
    print =<< Streams.toList is

setRecordIsRead :: MySQLConn -> Int -> IO ()
setRecordIsRead conn id = do
    s <- prepareStmt conn "UPDATE offers SET isRead = 1 WHERE ID = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Oznaczylem rekord" ++ show id ++ " jako przeczytany"
    print =<< Streams.toList is

setRecordIsInteresting :: MySQLConn -> Int -> IO ()
setRecordIsInteresting conn id = do
    s <- prepareStmt conn "UPDATE offers SET isInteresting = 1 WHERE ID = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Oznaczylem rekord " ++ show id ++ " jako interesujacy"
    print =<< Streams.toList is

addRecord :: MySQLConn -> Offer -> IO ()
addRecord conn offer = do
    s <- prepareStmt conn "INSERT INTO offers VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (defs, is) <- queryStmt conn s [MySQLText (pack $ title offer) , MySQLText (pack $ price offer), MySQLText (pack $ description offer), MySQLText (pack $ fromMaybe "brak" (priceFrom offer)), MySQLText (pack $ fromMaybe "brak" (priceTo offer)), MySQLText (pack $ fromMaybe "brak" (flatFrom offer)), MySQLText (pack $ fromMaybe "brak" (flatTo offer)), MySQLText (pack $ fromMaybe "brak" (roomsFrom offer)), MySQLText (pack $ fromMaybe "brak" (roomsTo offer)), MySQLInt32 0, MySQLInt32 0]
    putStrLn $ "Wstawilem rekord " ++ show offer
    print =<< Streams.toList is