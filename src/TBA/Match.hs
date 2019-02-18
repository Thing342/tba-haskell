{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TBA.Match where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Types

import Data.Traversable (for)
import Text.Read (readMaybe)
import Control.Monad (when, guard, liftM)
import Data.String (IsString)
import Data.EnumMap (EnumMap, fromList, lookup)

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Foldable as F

import qualified TBA.Games.DeepSpace as DS

data AllianceColor = Red | Blue deriving (Show, Eq, Ord, Enum)

data Match = Match {
    _redScore :: MatchScoreBreakdown
    , _blueScore :: MatchScoreBreakdown
    , _redTeams :: S.Set T.Text
    , _blueTeams :: S.Set T.Text
} deriving (Show)

boolId :: Value -> Parser Bool
boolId (Bool b) = return b
boolId _ = fail "expected bool"

enumMapFieldParse :: (Enum k) => [k] -> (k -> T.Text) -> (Value -> Parser a) -> Object -> Parser (EnumMap k a)
enumMapFieldParse enums keyfunc subparser o = let
    mapper enum = do
        let key = keyfunc enum
        val <- o .: key
        sub <- subparser val
        return (enum, sub)
    in (liftM fromList) ( mapM mapper enums )

deepSpaceParser :: Value -> Parser MatchScoreBreakdown
deepSpaceParser = withObject "2019 match breakdown object" $ \o -> do
    -- Simple Fields
    _adjustPoints     <- o .: "adjustPoints"
    _autoPoints       <- o .: "autoPoints"
    _cargoPoints      <- o .: "cargoPoints"
    _foulCount        <- o .: "foulCount"
    _foulPoints       <- o .: "foulPoints"
    _habClimbPoints   <- o .: "habClimbPoints"
    _hatchPanelPoints <- o .: "hatchPanelPoints"
    _rp               <- o .: "rp"
    _techFoulCount    <- o .: "techFoulCount"
    _teleopPoints     <- o .: "teleopPoints"
    _totalPoints      <- o .: "totalPoints"

    _completeRocketRankingPoint <- o .: "completeRocketRankingPoint"
    _habDockingRankingPoint     <- o .: "habDockingRankingPoint"
    _sandStormBonusPoints       <- o .: "sandStormBonusPoints"

    -- Complex map fields
    _completedRocket <- enumMapFieldParse [DS.RocketNear ..] DS.completedRocketKey boolId o
    _endgame <- enumMapFieldParse [DS.Robot1 ..] (\k -> T.pack ("endgame" ++ show k)) DS.parseHabLevel o
    _habLine <- enumMapFieldParse [DS.Robot1 ..] (\k -> T.pack ("habLine" ++ show k)) DS.parseHabLine o
    _preMatchLevel <- enumMapFieldParse [DS.Robot1 ..] (\k -> T.pack ("preMatchLevel" ++ show k)) DS.parseHabLevel o
    _scoring <- enumMapFieldParse [DS.Bay DS.Bay1 ..] DS.gamePiecePosKey DS.parseBayState o

    -- Custom parser
    _preMatchBay <- DS.preMatchBayParse o

    return DeepSpaceBreakdown{..}

instance FromJSON Match where
    parseJSON = withObject "match" $ \o -> do
        alliances <- o .: "alliances"
        redAlliance <- alliances .: "red"
        blueAlliance <- alliances .: "blue"
        _redTeams <- redAlliance .: "team_keys"
        _blueTeams <- blueAlliance .: "team_keys"

        scores <- o .: "score_breakdown"
        redScoreObj  <- scores .: "red"
        blueScoreObj <- scores .: "blue"
        _redScore <- deepSpaceParser redScoreObj
        _blueScore <- deepSpaceParser blueScoreObj
        return Match{..}

data MatchScoreBreakdown = 
    DeepSpaceBreakdown {
        _adjustPoints :: Int
        , _autoPoints :: Int
        , _cargoPoints :: Int
        , _completeRocketRankingPoint :: Bool
        , _completedRocket :: EnumMap DS.RocketSide Bool
        , _endgame :: EnumMap DS.RobotPos (Maybe DS.HabLevel)
        , _foulCount :: Int
        , _foulPoints :: Int
        , _habClimbPoints :: Int
        , _habDockingRankingPoint :: Bool
        , _habLine :: EnumMap DS.RobotPos (Maybe DS.HabLine)
        , _hatchPanelPoints :: Int
        , _preMatchBay :: EnumMap DS.BayPos(Maybe DS.BayState)
        , _preMatchLevel :: EnumMap DS.RobotPos (Maybe DS.HabLevel)
        , _rp :: Int
        , _sandStormBonusPoints :: Int
        , _scoring :: EnumMap DS.GamePiecePos (Maybe DS.BayState)
        , _techFoulCount :: Int
        , _teleopPoints :: Int
        , _totalPoints :: Int
    } |
    PowerUpBreakdown {
        _adjustPoints :: Int,
        _scalePoints :: Int
    } | 
    SteamworksBreakdown {
        _notAdjustPoints :: String
    } deriving (Show)

makePrisms ''MatchScoreBreakdown
makeLenses ''MatchScoreBreakdown
makeLenses ''Match