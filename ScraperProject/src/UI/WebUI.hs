{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BlockArguments #-}
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
import Control.Exception

type Server a = SpockM () () AppScrapingState a
data SetType = Interesting | NotInteresting | Read | NotRead

functions :: [String]
functions = ["Uruchomienie scrapowania (z możliwością określenia filtrów)", "Wyświetlenie historii wyszukiwań", "Oznaczenie oferty jako (nie)przeczytanej/jako (nie)interesującej" , "Wyświetlenie ofert oznaczonych jako interesujące"]

headersTable :: [String]
headersTable = ["ID", "Tytuł ogłoszenia", "Cena", "Opis", "CenaOd", "CenaDo", "PowOd", "PowDo", "PokojeOd", "PokojeDo", "Przeczytane", "Interesująca"]

scrapingTable :: [String]
scrapingTable = ["Tytuł ogłoszenia", "Cena", "Opis", "CenaOd", "CenaDo", "PowOd", "PowDo", "PokojeOd", "PokojeDo"]

runApp :: IO ()
runApp = do
    let st = createInitState 
    cfg <- defaultSpockCfg () PCNoDatabase st
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
            input_ [type_ "submit", value_ "Interesujące oferty", formaction_ "/inter"]
    get "scraper" $ lucid $ do
        h1_"Zdefiniowanie filtrów"
        p_ "Podanie filtrów niepoprawnych w sensie logicznym - przykładowo (Od większe od Do) lub podanie wartości bezsensownej zwróci pusty wynik. \n Jeśli nie chcesz wprowadzać filtrów, to po prostu naciśnij szukaj. \n Wynik scrapowania zostanie zapisany w bazie danych." >> createFilters
    post "results" $ do 
        conn <- getState
        priceFrom <- param' "priceFrom"
        priceTo <- param' "priceTo"
        flatFrom <- param' "flatFrom"
        flatTo <- param' "flatTo"
        roomsFrom <- param' "roomsFrom"
        roomsTo <- param' "roomsTo"
        offers <- liftIO $ titles (fromInput priceFrom) (fromInput priceTo) (fromInput flatFrom) (fromInput flatTo) (fromInput roomsFrom) (fromInput roomsTo) >>= correctOffers >>= offersToDisplay
        val <- liftIO $ catch ( liftIO conn >>= addAllRecords offers) handlingErrorInsert
        lucid $ do 
            h1_ "Wynik scrapowania"
            case val of
                [] -> p_ "Nie udał się zapis do bazy danych."
                _ -> p_ "Zapis do bazy danych udany."
            table_ $ (tr_ . mapM (th_ . toHtml)) scrapingTable >> mapM (tr_ . mapM (td_ . toHtml) ) offers >> moveToMain
    get "history" $ do
        conn <- getState
        offers <- liftIO $ catch (liftIO $ conn  >>= getOffers) handlingErrorDisplay
        lucid $ do 
            displayTable "Historia Scrapowania" offers >> h3_ "Oznacz jako (nie)interesujące:" >> interestingOffers >> h3_ "Oznacz jako (nie)przeczytane:" >> readOffers >> moveToMain
    post "setinter" $ changeRecordStatus Interesting
    post "setnotinter" $ changeRecordStatus NotInteresting
    post "setread" $ changeRecordStatus Read
    post "setnotread" $ changeRecordStatus NotRead
    get "inter" $ do
        conn <- getState 
        offers <- liftIO $ catch (liftIO $ conn  >>= getInterestingOffers) handlingErrorDisplay
        lucid $ displayTable "Interesujące ogłoszenia" offers >> moveToMain

displayTable :: Text -> [[String]] -> Html [[()]]
displayTable header toDisplay = do
    h1_ (toHtml header)
    table_ $ (tr_ . mapM (th_ . toHtml)) headersTable >> mapM (tr_ . mapM (td_ . toHtml) ) toDisplay

moveToMain :: Html ()
moveToMain = form_ [method_ "get"] $ do
    p_ "Aby wrócić do strony głównej naciśnij \"WSTECZ\""
    input_ [type_ "submit", value_ "WSTECZ", formaction_ "/"]

interestingOffers :: Html()
interestingOffers = form_ [method_ "post"] $ do
    p_ "Podaj ID i wykonaj akcję."
    label_ $ do
        "ID "
        input_ [name_ "idInter"]
    input_ [type_ "submit", value_ "Interesująca", formaction_ "/setinter"]
    input_ [type_ "submit", value_ "Nienteresująca", formaction_ "/setnotinter"]

readOffers :: Html()
readOffers = form_ [method_ "post"] $ do
    p_ "Podaj ID i wykonaj akcję."
    label_ $ do
        "ID "
        input_ [name_ "idRead"]
        input_ [type_ "submit", value_ "Przeczytana", formaction_ "/setread"]
        input_ [type_ "submit", value_ "Nieprzeczytana", formaction_ "/setnotread"]



changeRecordStatus :: SetType -> SpockActionCtx () () () AppScrapingState ()
changeRecordStatus NotInteresting = do
    inter <- param "idInter"
    conn <- getState 
    case inter of
        Just x -> liftIO $ catch (liftIO conn >>= setRecordIsInteresting x) handlingError
        Nothing -> liftIO $ putStrLn "Parameter is no available"
    redirect "/history"

changeRecordStatus Interesting = do
    inter <- param "idInter"
    conn <- getState 
    case inter of
        Just x -> liftIO $ catch ( liftIO $ conn >>= setRecordIsInteresting x) handlingError
        Nothing -> liftIO $ putStrLn "Parameter is no available"
    redirect "/history"

changeRecordStatus Read = do
    inter <- param "idRead"
    conn <- getState 
    case inter of
        Just x -> liftIO $ catch ( liftIO $ conn >>= setRecordIsRead x) handlingError
        Nothing -> liftIO $ putStrLn "Parameter is no available"
    redirect "/history"

changeRecordStatus NotRead = do
    inter <- param "idRead"
    conn <- getState 
    case inter of
        Just x -> liftIO $ catch (liftIO $ conn >>= setRecordIsNotRead x) handlingError 
        Nothing -> liftIO $ putStrLn "Parameter is no available"
    redirect "/history"

createFilters :: Html ()
createFilters = form_ [method_ "post"] $ do
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
    input_ [type_ "submit", value_ "Szukaj", formaction_ "/results"]

handlingErrorInsert :: SomeException -> IO [()]
handlingErrorInsert e = do 
    let err = show (e :: SomeException)
    putStrLn $ "Catch error " ++ err
    return []

handlingErrorDisplay :: SomeException -> IO [[String]]
handlingErrorDisplay e = do 
    let err = show (e :: SomeException)
    putStrLn $ "Catch error " ++ err
    return [["Nie udało się pobrać rekordów"]]

handlingError :: SomeException -> IO ()
handlingError e = do 
    let err = show (e :: SomeException)
    putStrLn $ "Catch error " ++ err
    