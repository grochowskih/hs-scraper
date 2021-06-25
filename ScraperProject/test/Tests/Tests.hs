module Tests.Tests where

import Test.QuickCheck
    ( elements, quickCheck, Arbitrary(arbitrary), Gen, Property )
import Test.QuickCheck.Monadic
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Backend.Scraper
import Control.Monad.IO.Class
import Control.Exception
import UI.WebUI
import GHC.Exception.Type
import Backend.DataModel
import Database.MySQL.Base
import Data.Text

instance Eq Offer where
    offer1 == offer2 = (title offer1 == title offer2) && (price offer1 == price offer2) && (description offer1 == description offer2) && (priceFrom offer1 == priceFrom offer2) && (priceTo offer1 == priceTo offer2) && (flatFrom offer1 == flatFrom offer2) && (flatTo offer1 == flatTo offer2) && (roomsFrom offer1 == roomsFrom offer2) && (roomsTo offer1 == roomsTo offer2)

instance Arbitrary Offer where
    arbitrary = genOffer 

instance Arbitrary MySQLValue where
    arbitrary = do
        val <- elements [MySQLText (pack "lama"), MySQLInt32 12, MySQLInt8 0, MySQLInt64 150]
        return $ val

genOffer :: Gen Offer 
genOffer = do
    title <- elements ["A", "B"]
    price <- elements ["2000"]
    description <- elements ["A", "B"]
    priceFrom <- elements [Just "2000", Nothing]
    priceTo <- elements [Just "3000", Nothing]
    flatFrom <- elements [Just "20",  Nothing]
    flatTo <- elements [Just "40", Nothing]
    roomsFrom <- elements [Just "2", Nothing]
    roomsTo <- elements [Just "6", Nothing]
    return $ Offer title price description priceFrom priceTo flatFrom flatTo roomsFrom roomsTo



prop_buildPriceFrom :: Maybe String -> Bool
prop_buildPriceFrom arg = case arg of
    Just x -> (buildPriceFrom arg) == "ps%5Bprice_from%5D=" ++ x ++ "&"
    Nothing -> (buildPriceFrom arg) == ""

prop_buildPriceTo :: Maybe String -> Bool
prop_buildPriceTo arg = case arg of
    Just x -> (buildPriceTo arg) == "ps%5Bprice_to%5D=" ++ x ++ "&"
    Nothing -> (buildPriceTo arg) == ""

prop_buildFlatFrom :: Maybe String -> Bool
prop_buildFlatFrom arg = case arg of
    Just x -> (buildFlatFrom arg) == "ps%5Bliving_area_from%5D=" ++ x ++ "&"
    Nothing -> (buildFlatFrom arg) == ""

prop_buildFlatTo :: Maybe String -> Bool
prop_buildFlatTo arg = case arg of
    Just x -> (buildFlatTo arg) == "ps%5Bliving_area_to%5D=" ++ x ++ "&"
    Nothing -> (buildFlatTo arg) == ""

prop_buildRoomsFrom :: Maybe String -> Bool
prop_buildRoomsFrom arg = case arg of
    Just x -> (buildRoomsFrom arg) == "?ps%5Bnumber_of_rooms_from%5D=" ++ x ++ "&"
    Nothing -> (buildRoomsFrom arg) == ""

prop_buildRoomsTo :: Maybe String -> Bool
prop_buildRoomsTo arg = case arg of
    Just x -> (buildRoomsTo arg) == "?ps%5Bnumber_of_rooms_to%5D=" ++ x ++ "&"
    Nothing -> (buildRoomsTo arg) == ""

prop_correctOffers :: Maybe [Offer] -> Property
prop_correctOffers (Just arg) = monadicIO $ do
    res <- run $ correctOffers (Just arg)
    Test.QuickCheck.Monadic.assert $ res == arg

prop_correctOffers Nothing = monadicIO $ do
    res <- run $ correctOffers Nothing 
    Test.QuickCheck.Monadic.assert $  res == []

prop_fromInput :: String -> Bool 
prop_fromInput x = case x of
    "" -> fromInput x == Nothing 
    _ -> fromInput x == Just x

prop_handlingErrorDisplay :: SomeException -> Property
prop_handlingErrorDisplay e = monadicIO $ do
    res <- run $ handlingErrorDisplay e
    Test.QuickCheck.Monadic.assert $ res == [["Nie udało się pobrać rekordów"]]

prop_handlingErrorInsert :: SomeException -> Property
prop_handlingErrorInsert e = monadicIO $ do
    res <- run $ handlingErrorDisplay e
    Test.QuickCheck.Monadic.assert $ res == []

prop_displayOffer :: Offer -> Bool 
prop_displayOffer offer = 
    (title offer) == (titleDisp res) && (price offer == priceDisp res) && (description offer == descriptionDisp res) && priceFromCond (priceFrom offer) && priceToCond (priceTo offer) && flatFromCond (flatFrom offer) && flatToCond (flatTo offer) && roomsFromCond (roomsFrom offer) && roomsToCond (roomsTo offer)
    where 
        res = displayOffer offer
        priceFromCond val = case val of
            Just x -> x == priceFromDisp res
            Nothing -> "" == priceFromDisp res
        priceToCond val = case val of
            Just x -> x == priceToDisp res
            Nothing -> "" == priceToDisp res
        flatFromCond val = case val of
            Just x -> x == flatFromDisp res
            Nothing -> "" == flatFromDisp res
        flatToCond val = case val of
            Just x -> x == flatToDisp res
            Nothing -> "" == flatToDisp res
        roomsFromCond val = case val of
            Just x -> x == roomsFromDisp res
            Nothing -> "" == roomsFromDisp res
        roomsToCond val = case val of
            Just x -> x == roomsToDisp res
            Nothing -> "" == roomsToDisp res

prop_mappingValueToString :: MySQLValue -> Bool
prop_mappingValueToString (MySQLText x) = mappingValueToString (MySQLText x) == ( show x )
prop_mappingValueToString (MySQLInt8 x) = mappingValueToString (MySQLInt8 x) == ( show x )
prop_mappingValueToString (MySQLInt32 x) = mappingValueToString (MySQLInt32 x) == ( show x )
prop_mappingValueToString x = mappingValueToString x == ( show x )


testAll = quickCheck prop_mappingValueToString >> quickCheck prop_displayOffer >> quickCheck prop_fromInput >> quickCheck prop_buildPriceFrom >> quickCheck prop_buildPriceTo >> quickCheck prop_buildFlatFrom >> quickCheck prop_buildFlatTo >> quickCheck prop_buildRoomsFrom >> quickCheck prop_buildRoomsTo >> quickCheck prop_fromInput >> quickCheck prop_correctOffers