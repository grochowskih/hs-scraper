module Backend.StateHandler where

import Backend.Scraper
import Database.MySQL.Base
import Backend.DAO
import Control.Monad.Trans.State.Strict
import System.IO
-- import System.Directory


type AppScrapingState = IO MySQLConn

-- |Inicjowanie połączenia wykorzystywane jako stan w UI.
createInitState :: AppScrapingState
createInitState = getPassword >>= createConnection

-- |Pobranie wartości hasła z pliku .txt.
getPasswordFromFile :: Handle -> IO String
getPasswordFromFile handle = do
    val <- hGetLine handle
    return val

-- |Obsłużenie pobrania wartości hasła.
getPassword :: IO String
getPassword = do
    withFile "resources/password.txt" ReadMode getPasswordFromFile