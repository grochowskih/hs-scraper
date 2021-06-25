module Backend.Scraper where

import Text.HTML.Scalpel

import Data.Aeson ( object, KeyValue((.=)), ToJSON(toJSON) )
import Data.Maybe

data Offer = Offer {
    title :: String,
    price :: String,
    description :: String,
    priceFrom :: Maybe String,
    priceTo :: Maybe String,
    flatFrom :: Maybe String,
    flatTo :: Maybe String,
    roomsFrom :: Maybe String,
    roomsTo :: Maybe String
}

data DisplayOffer = DisplayOffer {
    titleDisp :: String,
    priceDisp :: String,
    descriptionDisp :: String,
    priceFromDisp :: String,
    priceToDisp :: String,
    flatFromDisp :: String,
    flatToDisp :: String,
    roomsFromDisp :: String,
    roomsToDisp :: String
}

baseURL :: String
baseURL = "https://www.morizon.pl/do-wynajecia/mieszkania/najnowsze/warszawa/?"

instance Show Offer where
    show (Offer title price description _ _ _ _ _ _) = "Title: " ++ show title ++ " | Price: " ++ show price ++ " | Description : " ++ show description ++ " \n"

titles :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe [Offer])
titles priceFrom priceTo flatFrom flatTo roomsFrom roomsTo = scrapeURL (buildUrlScraper priceFrom priceTo flatFrom flatTo roomsFrom roomsTo) offers where
    offers:: Scraper String [Offer]
    offers = chroots (TagString "section" @: [hasClass "single-result__content"]) offer

    offer :: Scraper String Offer
    offer = do
        title <- text $ TagString "h2" @: [hasClass "single-result__title"]
        price <- text $ TagString "p"  @: [hasClass "single-result__price"]
        description <- text $ TagString "div" @: [hasClass "single-result__description"]
        return $ Offer title price description priceFrom priceTo flatFrom flatTo roomsFrom roomsTo

correctOffers :: Maybe [Offer] -> IO [Offer]
correctOffers arg = return $ fromMaybe [] arg

buildPriceFrom :: Maybe String -> String
buildPriceFrom val = case val of
    Just v -> "ps%5Bprice_from%5D=" ++ v ++ "&"
    Nothing -> ""

buildPriceTo :: Maybe String -> String
buildPriceTo val = case val of
    Just v -> "ps%5Bprice_to%5D=" ++ v ++ "&"
    Nothing -> ""

buildFlatFrom :: Maybe String -> String
buildFlatFrom val = case val of
    Just v -> "ps%5Bliving_area_from%5D=" ++ v ++ "&"
    Nothing -> ""

buildFlatTo :: Maybe String -> String
buildFlatTo val = case val of
    Just v -> "ps%5Bliving_area_to%5D=" ++ v ++ "&"
    Nothing -> ""

buildRoomsFrom :: Maybe String -> String
buildRoomsFrom val = case val of
    Just v -> "?ps%5Bnumber_of_rooms_from%5D=" ++ v ++ "&"
    Nothing -> ""

buildRoomsTo :: Maybe String -> String
buildRoomsTo val = case val of
    Just v -> "?ps%5Bnumber_of_rooms_to%5D=" ++ v ++ "&"
    Nothing -> ""

buildUrlScraper :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> String
buildUrlScraper priceFrom priceTo flatFrom flatTo roomsFrom roomsTo = init $ baseURL ++ buildPriceFrom priceFrom ++ buildPriceTo priceTo ++ buildFlatFrom flatFrom ++ buildFlatTo flatTo ++ buildRoomsFrom roomsFrom ++ buildRoomsTo roomsTo

fromInput :: String -> Maybe String
fromInput x =
    case x of 
        "" -> Nothing
        _ -> Just x

offersToDisplay :: [Offer] -> IO [[String]]
offersToDisplay offers = return $ map (offerToString . displayOffer) offers

displayOffer :: Offer -> DisplayOffer
displayOffer offer = DisplayOffer (title offer) (price offer) (description offer) (fromMaybe "" (priceFrom offer)) (fromMaybe "" (priceTo offer)) (fromMaybe "" (flatFrom offer)) (fromMaybe "" (flatTo offer)) (fromMaybe "" (roomsFrom offer)) (fromMaybe "" (roomsTo offer) )

offerToString :: DisplayOffer -> [String]
offerToString arg = [titleDisp arg, priceDisp arg, descriptionDisp arg, priceFromDisp arg, priceToDisp arg, flatFromDisp arg, flatToDisp arg, roomsFromDisp arg, roomsToDisp arg]