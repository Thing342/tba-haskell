{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import TBA.Match
import TBA.Games.DeepSpace

import Control.Lens
import Network.Wreq (responseBody)

main :: IO ()
main = do
    putStrLn "Enter code of event to get scores: "
    event <- getLine
    matches <- decodeMatches event
    let scores = matches ^.. responseBody . traverse . (redScore <> blueScore) . totalPoints
    putStrLn $ "Scores: " ++ (show scores)
    putStrLn $ "Total: " ++  (show $ sum scores)