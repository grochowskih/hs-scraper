module Main where

import Backend.Scraper
import Prelude
import Backend.DAO
import UI.WebUI

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
-- main = createConnection >>= getOffers

main = runApp