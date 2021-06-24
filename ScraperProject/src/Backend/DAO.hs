{-# LANGUAGE OverloadedStrings #-}
module Backend.DAO where

import Database.MySQL.Base
import Database.MySQL.Connection
import System.IO.Streams as Streams
import Data.Int
import Backend.Scraper
import Data.Maybe
import Data.Text
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Backend.DataModel

run :: (StateT MySQLConn IO) ()
run = do
    res <- liftIO createConnection
    modify (\el -> res)

createConnection :: IO MySQLConn 
createConnection = do
    conn <- Database.MySQL.Base.connect
    putStrLn "Nawiazalem polaczenie"
    return conn

getOffers :: MySQLConn -> IO [[String]] -- IO [[MySQLValue]]
getOffers conn = do
    (defs, is) <- query_ conn "SELECT * FROM offers"
    result <- liftIO $ Streams.toList is 
    return $ mappingValuesToList result

getInterestingOffers :: MySQLConn -> IO [[String]] -- IO [[MySQLValue]]
getInterestingOffers conn = do
    (defs, is) <- query_ conn "SELECT * FROM offers WHERE isInteresting = 1 "
    result <- liftIO $ Streams.toList is 
    return $ mappingValuesToList result

setRecordIsRead :: Int -> MySQLConn -> IO ()
setRecordIsRead id conn = do
    s <- prepareStmt conn "UPDATE offers SET isRead = 1 WHERE ID = ?"
    putStrLn "Try to execute setRecordIsRead"
    executeStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Set record with id = " ++ show id ++ " isRead"

setRecordIsInteresting :: Int -> MySQLConn -> IO ()
setRecordIsInteresting id conn= do
    s <- prepareStmt conn "UPDATE offers SET isInteresting = 1 WHERE ID = ?"
    putStrLn "Try to execute setRecordIsInteresting"
    executeStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Set record with id = " ++ show id ++ " is interesting"

setRecordIsNotInteresting :: Int -> MySQLConn -> IO ()
setRecordIsNotInteresting id conn= do
    s <- prepareStmt conn "UPDATE offers SET isInteresting = 0 WHERE ID = ?"
    putStrLn "Try to execute setRecordIsInteresting"
    executeStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Set record with id = " ++ show id ++ " is not interesting"

addRecord :: MySQLConn -> Offer -> IO ()
addRecord conn offer = do
    s <- prepareStmt conn "INSERT INTO offers VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (defs, is) <- queryStmt conn s [MySQLText (pack $ title offer) , MySQLText (pack $ price offer), MySQLText (pack $ description offer), MySQLText (pack $ fromMaybe "brak" (priceFrom offer)), MySQLText (pack $ fromMaybe "brak" (priceTo offer)), MySQLText (pack $ fromMaybe "brak" (flatFrom offer)), MySQLText (pack $ fromMaybe "brak" (flatTo offer)), MySQLText (pack $ fromMaybe "brak" (roomsFrom offer)), MySQLText (pack $ fromMaybe "brak" (roomsTo offer)), MySQLInt32 0, MySQLInt32 0]
    putStrLn $ "Wstawilem rekord " ++ show offer
    print =<< Streams.toList is