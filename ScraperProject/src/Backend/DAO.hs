{-# LANGUAGE OverloadedStrings #-}
module Backend.DAO where

import Database.MySQL.Base
import System.IO.Streams as Streams
import Data.Int

testConnect :: IO MySQLConn 
testConnect = do
    conn <- Database.MySQL.Base.connect
        
    putStrLn "Nawiazalem polaczenie"
    return conn

getOffers :: MySQLConn -> IO ()
getOffers conn = do
    (defs, is) <- query_ conn "SELECT * FROM offers"
    putStrLn "Wykonalem selecta"
    print =<< Streams.toList is

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