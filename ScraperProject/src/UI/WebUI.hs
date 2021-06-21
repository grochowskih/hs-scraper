{-# LANGUAGE OverloadedStrings #-}
module UI.WebUI where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid
import Lucid
import Backend.StateHandler
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Backend.DAO
import Control.Monad (forM_)


type Server a = SpockM () () AppScrapingState a

functions :: [String]
functions = ["Uruchomienie scrapowania (z możliwością określenia filtów)", "Wyświetlenie historii wyszukiwań", "Oznaczenie oferty jako przeczytanej/jako interesującej" , "Wyświetlenie ofert oznaczonych jako interesujące"]

runApp :: IO ()
runApp = do
    let st = createInitState 
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)

app :: Server ()
app = do
    get root $ lucid $ do
        h1_ "Scrapowanie mieszkań"
        p_ "Aplikacja służy do scrapowania mieszkań z pierwszej strony udostępnionych do wynajęcia w Warszawie na stornie Morizon. Dostępne funkcjonalności:"
        ul_ $ mapM (li_ . toHtml) functions
        h2_ "Wybierz co chcesz wykonać"
        form_ [method_ "get"] $ do
            input_ [type_ "submit", value_ "Scrapuj", formaction_ "/scraper"]
            input_ [type_ "submit", value_ "Historia", formaction_ "/history"]
            input_ [type_ "submit", value_ "Wyróżnione oferty", formaction_ "/inter"]
    get "scraper" $ lucid $ do
        h1_"tu bedzie scrapowanie"
    get "history" $ lucid $ do
        h1_ "Historia scrapowania"
        -- offers <- liftIO $ createConnection >>= getOffers
        -- ul_ $ map (map (p_ . toHtml)) offers
    get "inter" $ lucid $ do
        h1_"tu bedzie interesujace"
        


