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


createConnection :: IO MySQLConn 
createConnection = do
    conn <- Database.MySQL.Base.connect
    putStrLn "Created connection with DB"
    return conn

getOffers :: MySQLConn -> IO [[String]] -- IO [[MySQLValue]]
getOffers conn = do
    putStrLn $ "Try to execute select all offers"
    (defs, is) <- query_ conn "SELECT * FROM offers"
    result <- liftIO $ Streams.toList is 
    putStrLn "Executed select all offers"
    return $ mappingValuesToList result

getInterestingOffers :: MySQLConn -> IO [[String]] -- IO [[MySQLValue]]
getInterestingOffers conn = do
    putStrLn $ "Try to execute select all interesting offers"
    (defs, is) <- query_ conn "SELECT * FROM offers WHERE isInteresting = 1 "
    result <- liftIO $ Streams.toList is 
    putStrLn "Executed select all interestinv offers"
    return $ mappingValuesToList result

setRecordIsRead :: Int -> MySQLConn -> IO ()
setRecordIsRead id conn = do
    putStrLn "Try to execute setRecordIsRead"
    s <- prepareStmt conn "UPDATE offers SET isRead = 1 WHERE ID = ?"
    executeStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Set record with id = " ++ show id ++ " isRead"

setRecordIsNotRead :: Int -> MySQLConn -> IO ()
setRecordIsNotRead id conn = do
    putStrLn "Try to execute setRecordIsNotRead"
    s <- prepareStmt conn "UPDATE offers SET isRead = 0 WHERE ID = ?"
    executeStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Set record with id = " ++ show id ++ " is not Read"

setRecordIsInteresting :: Int -> MySQLConn -> IO ()
setRecordIsInteresting id conn= do
    putStrLn "Try to execute setRecordIsInteresting"
    s <- prepareStmt conn "UPDATE offers SET isInteresting = 1 WHERE ID = ?"
    executeStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Set record with id = " ++ show id ++ " is interesting"

setRecordIsNotInteresting :: Int -> MySQLConn -> IO ()
setRecordIsNotInteresting id conn= do
    putStrLn "Try to execute setRecordIsInteresting"
    s <- prepareStmt conn "UPDATE offers SET isInteresting = 0 WHERE ID = ?"
    executeStmt conn s [MySQLInt32 (fromIntegral id)]
    putStrLn $ "Set record with id = " ++ show id ++ " is not interesting"

addRecord ::  MySQLConn -> [String] -> IO ()
addRecord conn offer = do
    putStrLn "Try to add record"
    s <- prepareStmt conn "INSERT INTO offers (title, price, description, priceFrom, priceTo, flatFrom, flatTo, roomsFrom, roomsTo, isRead, isInteresting) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    executeStmt conn s ( Prelude.map (MySQLText . pack) offer++[MySQLInt8 0, MySQLInt8 0] )
    putStrLn $ "AddRecord with offer " ++ show (Prelude.head offer) ++ " success"

addAllRecords :: [[String]] ->  MySQLConn -> IO [()]
addAllRecords offers conn = Prelude.mapM (addRecord conn) offers