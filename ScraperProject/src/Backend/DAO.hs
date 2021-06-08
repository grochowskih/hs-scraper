{-# LANGUAGE OverloadedStrings #-}
module Backend.DAO where

import Database.MySQL.Base
    ( query_,
      connect,
      defaultConnectInfo,
      ConnectInfo(ciHost, ciPort, ciUser, ciPassword, ciDatabase) )
import System.IO.Streams as Streams

testConnect :: IO () 
testConnect = do
    conn <- Database.MySQL.Base.connect
        defaultConnectInfo {ciHost = "sql11.freesqldatabase.com", ciPort = 3306, ciUser = "sql11417464", ciPassword = "XDMEcWeuRp", ciDatabase = "sql11417464"}
    (defs, is) <- query_ conn "SELECT * FROM offers"
    print =<< Streams.toList is