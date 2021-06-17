{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where

import Backend.Scraper
import Prelude
import Backend.DAO

main :: IO ()
--main = do
--    putStrLn "Cena od: "
--    priceFrom <- getLine
--    putStrLn "Cena do: "
--    priceTo <- getLine 
--    res <- titles (Just priceFrom) (Just priceTo) Nothing Nothing Nothing Nothing
--    case res of 
--        Just toShow -> print toShow
--        Nothing -> undefined
main = testConnect >>= getOffers