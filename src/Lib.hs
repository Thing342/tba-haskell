{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Network.Wreq
import Control.Lens
import Data.Text hiding (foldr)
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens (_String, _Double, AsValue, key, values, nth)
import Data.Scientific

import TBA.Match

data Color = Red | Blue

type KeyLens v t f = (v -> f v) -> t -> f t

tba_req auth path = let
    params = defaults & param "X-TBA-Auth-Key" .~ [auth]
    url = "https://www.thebluealliance.com/api/v3/" ++ path
    in (putStrLn url) >> (getWith params url)

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = let 
    acc = (\next (sum, count) -> (sum + next, count + 1))
    (sum, count) = foldr acc (0,0) xs 
    in sum / count

tba = tba_req "CggHG7Eq2A8BFPYIEZ6x1Al61t3Q7aKUFep6slIuzwX0zkZvdJFjvvU28IkQi4ay"

eventMatches event = tba $ "event/" ++ event ++ "/matches"

_alliance :: (AsValue t, Applicative f) => Color -> KeyLens Value t f
_alliance Red = key "red"
_alliance Blue = key "blue"

_score :: (AsValue t, Applicative f) => Color -> KeyLens Double t f
_score color = key "alliances" . _alliance color . key "score" . _Double

_score_breakdown color = key "score_breakdown" . _alliance color

averageScores :: String -> IO Double
averageScores event = do
    doc <- eventMatches event
    let scores = doc ^.. responseBody . values . (_score Red <> _score Blue)
    return (mean scores)

eventScoreItemMean lens event = do
    doc <- eventMatches event
    let scores = doc ^.. responseBody . values . ((_score_breakdown Red . lens) <> (_score_breakdown Blue . lens))
    return (mean scores)

decodeMatches :: String -> IO (Response [Match])
decodeMatches event = do
    doc <- eventMatches event
    asJSON doc