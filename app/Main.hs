{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib (mean)

import TBA
import TBA.Match
import TBA.Games.DeepSpace

import Control.Lens
import Network.Wreq (responseBody)

import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Enter auth code: "
    auth <- getLine
    client <- tbaNew $ T.pack auth
    
    putStrLn "Enter code of event to get scores: "
    event <- getLine
    matches <- tbaEventMatches event client

    let scores = matches ^.. responseBody . traverse . (redScore <> blueScore) . totalPoints
    let scoresF = fromIntegral <$> scores

    putStrLn $ "Scores: " ++ (show scores)
    putStrLn $ "Total: " ++  (show $ sum scoresF)
    putStrLn $ "Average: " ++ (show $ mean scoresF)