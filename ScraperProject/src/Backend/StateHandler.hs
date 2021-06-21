module Backend.StateHandler where

import Backend.Scraper
import Database.MySQL.Base
import Backend.DAO
import Control.Monad.Trans.State.Strict
import Data.IORef

newtype AppScrapingState = AppScrapingState (IO MySQLConn, [Offer])

--executeScrapping :: (StateT AppState IO) [Offer]
--executeScrapping = do 

createInitState :: AppScrapingState
createInitState = AppScrapingState (createConnection, [])
