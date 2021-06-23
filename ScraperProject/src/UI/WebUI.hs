{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module UI.WebUI where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid
import Lucid
import Backend.StateHandler
import Data.IORef
import Backend.DAO
import Control.Monad (forM_)
-- import Web.Spock.Internal.Types
import Control.Monad.IO.Class
import Data.Text
import Backend.Scraper
import Data.Maybe

type Server a = SpockM () () AppScrapingState a

functions :: [String]
functions = ["Uruchomienie scrapowania (z możliwością określenia filtów)", "Wyświetlenie historii wyszukiwań", "Oznaczenie oferty jako przeczytanej/jako interesującej" , "Wyświetlenie ofert oznaczonych jako interesujące"]

headersTable :: [String]
headersTable = ["ID|", "Tytuł ogłoszenia|", "Cena|", "Opis|", "CenaOd|", "CenaDo|", "PowOd|", "PowDo|", "PokojeOd|", "PokojeDo|", "Przeczytane|", "Interesująca"]

runApp :: IO ()
runApp = do
    let st = createInitState 
    cfg <- defaultSpockCfg () PCNoDatabase st
    -- offers <- createConnection >>= getOffers
    runSpock 8080 (spock cfg app)

app :: Server () -- SpockM () () AppScrapingState a
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
        h1_"Zdefiniowanie filtrów"
        form_ [method_ "post"] $ do
                p_ "Podaj filtry, które chcesz zdefiniować."
                label_ $ do
                    "Cena od: "
                    input_ [name_ "priceFrom"]
                label_ $ do
                    "Cena do: "
                    input_ [name_ "priceTo"]
                label_ $ do
                    "Powierzchnia od: "
                    input_ [name_ "flatFrom"]
                label_ $ do
                    "Powierzchnia do: "
                    input_ [name_ "flatTo"]
                label_ $ do
                    "Liczba pokoi od: "
                    input_ [name_ "roomsFrom"]
                label_ $ do
                    "Liczba pokoi do: "
                    input_ [name_ "roomsTo"]
                input_ [type_ "submit", value_ "Wprowadź filtry", formaction_ "/results"]
    post "results" $ do 
        priceFrom <- param' "priceFrom"
        priceTo <- param' "priceTo"
        flatFrom <- param' "flatFrom"
        flatTo <- param' "flatTo"
        roomsFrom <- param' "roomsFrom"
        roomsTo <- param' "roomsTo"
        offers <- liftIO $ (titles (fromInput priceFrom) (fromInput priceTo) (fromInput flatFrom) (fromInput flatTo) (fromInput roomsFrom) (fromInput roomsTo)) >>= correctOffers
        lucid $ do 
            h1_ "Wynik scrapowania"
            --table_ $ mapM (tr_ . toHtml) offers
    get "history" $ do
        offers <- liftIO $ createConnection  >>= getOffers
        lucid $ do 
            h1_ "Historia scrapowania"
            table_ $ (tr_ . mapM (th_ . toHtml)) headersTable >> mapM (tr_ . mapM (td_ . toHtml) ) offers
            h3_ "Oznacz jako przeczytane:"
            form_ [method_ "post"] $ do
                p_ "Podaj ID ogłoszenia, które chcesz oznaczysz jako przeczytane."
                label_ $ do
                    "ID "
                    input_ [name_ "idRead"]
                input_ [type_ "submit", value_ "Oznacz jako przeczytane"]
    post "history" $ do
        inter <- param' "idRead"
        liftIO $ createConnection >>= setRecordIsRead inter
        redirect "/history"
    get "inter" $ do
        offers <- liftIO $ createConnection  >>= getInterestingOffers
        lucid $ do 
            h1_ "Interesujące ogłoszenia"
            table_ $ (tr_ . mapM (th_ . toHtml)) headersTable >> mapM (tr_ . mapM (td_ . toHtml) ) offers