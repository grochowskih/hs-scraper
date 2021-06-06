{-# LANGUAGE OverloadedStrings #-}
module Backend.JsonHandler where

import Backend.Scraper
import Data.Aeson

instance ToJSON Offer where
 toJSON (Offer title price description priceFrom priceTo flatFrom flatTo roomsFrom roomsTo) =
    object [ "title"  .= title
           , "price"   .= price
           , "description"        .= description
           , "priceFrom" .= priceFrom
           , "priceTo" .= priceTo
           , "flatFrom" .= flatFrom
           , "flatTo" .= flatTo
           , "roomsFrom" .= roomsFrom
           , "roomsTo" .= roomsTo
             ]

instance FromJSON Offer where
 parseJSON (Object v) =
    Offer <$> v .: "title"
           <*> v .:  "price"
           <*> v .:  "description"
           <*> v .:  "priceFrom"
           <*> v .:  "priceTo"
           <*> v .:  "flatFrom"
           <*> v .:  "flatTo"
           <*> v .:  "roomsFrom"
           <*> v .:  "roomsTo"