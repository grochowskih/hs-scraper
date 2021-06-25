module Backend.StateHandler where

import Backend.Scraper
import Database.MySQL.Base
import Backend.DAO
import Control.Monad.Trans.State.Strict
import System.IO
-- import System.Directory


type AppScrapingState = IO MySQLConn

createInitState :: AppScrapingState
createInitState = getPassword >>= createConnection

getPasswordFromFile :: Handle -> IO String
getPasswordFromFile handle = do
    val <- hGetLine handle
    putStrLn val
    return val

getPassword :: IO String
getPassword = do
    withFile "resources/password.txt" ReadMode getPasswordFromFile