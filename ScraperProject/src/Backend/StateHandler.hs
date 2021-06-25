module Backend.StateHandler where

import Backend.Scraper
import Database.MySQL.Base
import Backend.DAO
import Control.Monad.Trans.State.Strict
import Data.IORef

type AppScrapingState = IO MySQLConn

--executeScrapping :: (StateT AppState IO) [Offer]
--executeScrapping = do 

createInitState :: AppScrapingState
createInitState = createConnection
