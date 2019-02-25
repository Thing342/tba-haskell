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
import Control.Lens.Prism(Prism, prism)
import Data.String (IsString)
import Data.EnumMap (EnumMap, fromList, lookup)

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Foldable as F

import TBA.Enums
import qualified TBA.Games.DeepSpace as DS
import qualified TBA.Games.PowerUp as PU

type MatchTime = Integer
data Match = Match {
    _matchKey :: T.Text,
    _compLevel :: CompetitionLevel,
    _setNumber :: Integer,
    _matchNumber :: Integer,
    _winningAlliance :: MatchWinner,
    _eventKey :: T.Text,
    _scheduledTime :: MatchTime,
    _predictedTime :: MatchTime,
    _actualTime :: MatchTime,
    _resultsPostTime :: MatchTime,
    _redScore :: MatchScoreBreakdown,
    _blueScore :: MatchScoreBreakdown,
    _redTeams :: S.Set T.Text,
    _blueTeams :: S.Set T.Text
} deriving (Show)

-- Parsing utils --

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

enumMapFieldParseJSON :: (Enum k, FromJSON a) => [k] -> (k -> T.Text) -> Object -> Parser (EnumMap k a)
enumMapFieldParseJSON enums keyfunc o = let
    mapper enum = do
        let key = keyfunc enum
        val <- o .: key
        return (enum, val)
    in (liftM fromList) ( mapM mapper enums )

-- Match Score Parsers --

type MatchScoreBreakdownParser = Value -> Parser MatchScoreBreakdown

deepSpaceParser :: MatchScoreBreakdownParser
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

powerUpParser :: MatchScoreBreakdownParser
powerUpParser = withObject "2018 match breakdown object" $ \o -> do
    -- Simple Fields
    _adjustPoints     <- o .: "adjustPoints" :: Parser Int
    _autoPoints       <- o .: "autoPoints" :: Parser Int
    _foulCount        <- o .: "foulCount" :: Parser Int
    _foulPoints       <- o .: "foulPoints" :: Parser Int
    _rp               <- o .: "rp" :: Parser Int
    _techFoulCount    <- o .: "techFoulCount" :: Parser Int
    _teleopPoints     <- o .: "teleopPoints" :: Parser Int
    _totalPoints      <- o .: "totalPoints" :: Parser Int

    _autoSwitchOwnershipSec <- o .: "autoSwitchOwnershipSec"
    _autoScaleOwnershipSec <- o .: "autoScaleOwnershipSec"
    _autoSwitchAtZero <- o .: "autoSwitchAtZero"
    _autoRunPoints <- o .: "autoRunPoints"
    _autoOwnershipPoints <- o .: "autoOwnershipPoints"
    _teleopSwitchOwnershipSec <- o .: "teleopSwitchOwnershipSec"
    _teleopSwitchBoostSec <- o .: "teleopSwitchBoostSec"
    _teleopSwitchForceSec <- o .:"teleopSwitchForceSec"
    _teleopScaleOwnershipSec <- o .: "teleopScaleOwnershipSec"
    _teleopScaleBoostSec <- o .: "teleopScaleBoostSec"
    _teleopScaleForceSec <- o .:"teleopScaleForceSec"
    _teleopOwnershipPoints <- o .: "teleopOwnershipPoints"
    _teleopPoints <- o .: "teleopPoints"
    _vaultForceTotal <- o .: "vaultForceTotal"
    _vaultForcePlayed <- o .: "vaultForcePlayed"
    _vaultBoostTotal <- o .: "vaultBoostTotal"
    _vaultBoostPlayed <- o .: "vaultBoostPlayed"
    _vaultLevitatePlayed <- o .: "vaultLevitatePlayed"
    _vaultPoints <- o .: "vaultPoints"
    _autoQuestRankingPoint <- o .: "autoQuestRankingPoint"
    _faceTheBossRankingPoint <- o .: "faceTheBossRankingPoint"
    _endgamePoints <- o .: "endgamePoints"

    -- Complex Map Fields
    _auto2018 <- enumMapFieldParseJSON [PU.Robot1 ..]  (\k -> T.pack ("auto" ++ show k)) o
    _endgame2018 <- enumMapFieldParseJSON [PU.Robot1 ..]  (\k -> T.pack ("endgame" ++ show k)) o

    return PowerUpBreakdown{..}

defaultParser :: T.Text -> MatchScoreBreakdownParser
defaultParser year = withObject "match breakdown object" $ \o -> do
    let _year = year
    let _otherFields = o
    _totalPoints <- o .: "totalPoints" :: Parser Int
    return DefaultBreakdown{..}

chooseParser :: T.Text -> MatchScoreBreakdownParser
chooseParser eventKey = case T.take 4 eventKey of
    "2019" -> deepSpaceParser
    "2018" -> powerUpParser
    year -> defaultParser year

-- Match FromJSON --

instance FromJSON Match where
    parseJSON = withObject "match" $ \o -> do
        _matchKey <- o .: "key"
        _compLevel <- o .: "comp_level"
        _setNumber <- o .: "set_number"
        _matchNumber <- o .: "match_number"
        _winningAlliance <- o .: "winning_alliance"
        _eventKey <- o .: "event_key"
        _scheduledTime <- o .: "time"
        _predictedTime <- o .: "predicted_time"
        _actualTime <- o .: "actual_time"
        _resultsPostTime <- o .: "post_result_time"

        alliances <- o .: "alliances"
        redAlliance <- alliances .: "red"
        blueAlliance <- alliances .: "blue"
        _redTeams <- redAlliance .: "team_keys"
        _blueTeams <- blueAlliance .: "team_keys"

        let breakdownParser = chooseParser _eventKey

        scores <- o .: "score_breakdown"
        redScoreObj  <- scores .: "red"
        blueScoreObj <- scores .: "blue"
        _redScore <- breakdownParser redScoreObj
        _blueScore <- breakdownParser blueScoreObj
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
        _auto2018 :: EnumMap PU.RobotPos PU.AutoAction,
        _autoSwitchOwnershipSec :: Int,
        _autoScaleOwnershipSec :: Int,
        _autoSwitchAtZero :: Bool,
        _autoRunPoints :: Int,
        _autoOwnershipPoints :: Int,
        _autoPoints :: Int,
        _teleopSwitchOwnershipSec :: Int,
        _teleopSwitchBoostSec :: Int,
        _teleopSwitchForceSec :: Int,
        _teleopScaleOwnershipSec :: Int,
        _teleopScaleBoostSec :: Int,
        _teleopScaleForceSec :: Int,
        _teleopOwnershipPoints :: Int,
        _teleopPoints :: Int,
        _vaultForceTotal :: Int,
        _vaultForcePlayed :: Int,
        _vaultLevitatePlayed :: Int,
        _vaultBoostTotal :: Int,
        _vaultBoostPlayed :: Int,
        _vaultPoints :: Int,
        _endgame2018 :: EnumMap PU.RobotPos PU.EndgameAction,
        _endgamePoints :: Int,
        _autoQuestRankingPoint :: Bool,
        _faceTheBossRankingPoint :: Bool,
        _foulCount :: Int,
        _techFoulCount :: Int,
        _foulPoints :: Int,
        _rp :: Int,
        _totalPoints :: Int
    } | 
    DefaultBreakdown {
        _year :: T.Text,
        _totalPoints :: Int,
        _otherFields :: Object
    } deriving (Show)

makePrisms ''MatchScoreBreakdown
makeLenses ''MatchScoreBreakdown
makeLenses ''Match

-- Custom Lenses --

_teamScore :: T.Text -> Match -> Maybe MatchScoreBreakdown
_teamScore team match = let
    isRed = S.member team $ _redTeams match
    isBlue = S.member team $ _blueTeams match
    in case (isRed, isBlue) of
        (True, _) -> Just $ _redScore match
        (_, True) -> Just $ _blueScore match
        _ -> Nothing

teamScore :: (Contravariant f, Choice p, Applicative f) =>
    T.Text
    -> p MatchScoreBreakdown (f MatchScoreBreakdown)
    -> p Match (f Match)
teamScore team = to (_teamScore team ) . _Just