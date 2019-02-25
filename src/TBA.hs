{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TBA where
 
import TBA.Match
import TBA.District

import Network.Wreq

import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens (_String, _Double, _Integer, AsValue, key, values, nth)
import Data.Scientific
import Data.Traversable (for)

import qualified Network.Wreq.Session as S
import qualified Data.Text as T
import qualified Data.Set as DS

data TBAClient = TBAClient {
    tbaAuth :: T.Text
    , tbaSession :: S.Session
}

type TBARequest a = TBAClient -> IO a

tbaNew :: T.Text -> IO TBAClient
tbaNew tbaAuth = do
    tbaSession <- S.newSession
    return TBAClient{..}

tbaParams client = defaults & param "X-TBA-Auth-Key" .~ [tbaAuth client]

tbaRequest :: (FromJSON a) => String -> TBARequest a
tbaRequest path client = do
    let params = tbaParams client
    let sess = tbaSession client
    let url = "https://www.thebluealliance.com/api/v3/" ++ path
    
    -- echo url to console for debugging
    putStrLn url

    -- make request and parse as json
    resp <- S.getWith params sess url
    jsonResp <- asJSON resp
    return $ jsonResp ^. responseBody

tbaDistrictEventNames :: Int -> District -> TBARequest [T.Text]
tbaDistrictEventNames year district client = do
    let dName = districtCode year district
    let url = "district/" ++ dName ++ "/events/keys"
    tbaRequest url client

tbaDistrictMatches :: Int -> District -> TBARequest [Match]
tbaDistrictMatches year district client = do
    eventKeys <- tbaDistrictEventNames year district client
    matches_nested <- for eventKeys $ \key -> tbaEventMatches (T.unpack key) client
    return $ join matches_nested

tbaEventMatches :: String -> TBARequest [Match]
tbaEventMatches eventCode = tbaRequest ("event/" ++ eventCode ++ "/matches")

tbaSingleMatch :: String -> TBARequest Match
tbaSingleMatch matchKey = tbaRequest ("match/" ++ matchKey)

teamNumSet :: Int -> District -> TBARequest (DS.Set Integer)
teamNumSet year district client = do
    let dName = districtCode year district
    let path = "district/" ++ dName ++ "/teams"
    dTeamsJson <- tbaRequest path client :: IO Array
    let dSet = DS.fromList $ dTeamsJson ^.. traverse . key "team_number" . _Integer
    return dSet