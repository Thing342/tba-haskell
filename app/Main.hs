{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib (mean)

import TBA
import TBA.Match
import TBA.Games.DeepSpace

import Control.Lens
import Data.Aeson (Array)
import Data.Aeson.Lens (key, _Integer)
import Network.Wreq (responseBody, Response)

import qualified Data.Text as T
import qualified Data.Set as S

main :: IO()
main = showTeamAttrition

showAverageScores :: IO ()
showAverageScores = do
    client <- createClient
    event <- ask "Enter code of event to get scores: "
    
    matches <- tbaEventMatches event client

    let scores = matches ^.. responseBody . traverse . (redScore <> blueScore) . totalPoints
    let scoresF = fromIntegral <$> scores

    putStrLn $ "Scores: " ++ (show scores)
    putStrLn $ "Total: " ++  (show $ sum scoresF)
    putStrLn $ "Average: " ++ (show $ mean scoresF)

showTeamAttrition :: IO ()
showTeamAttrition = do
    client <- createClient
    d1Name <- ask "Enter old district code: (e.g. 2016fim)"
    d2Name <- ask "Enter new district code: (e.g. 2017fim)"

    d1Set <- teamNumSet d1Name client
    d2Set <- teamNumSet d2Name client

    let attrition = d1Set `S.difference` d2Set
    let rookies = d2Set `S.difference` d1Set

    putStrLn $ "Attrition: " ++ (show attrition)
    putStrLn $ "Rookies: " ++ (show rookies)

ask :: String -> IO String
ask question = do
    putStrLn question
    getLine

createClient :: IO TBAClient
createClient = do
    auth <- ask "Enter auth code: "
    tbaNew $ T.pack auth