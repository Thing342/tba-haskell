{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TBA.Games.PowerUp where

import Data.Aeson
import Data.Aeson.Types
import Data.Text

import Text.Read (readMaybe)

data RobotPos = Robot1 | Robot2 | Robot3 deriving (Show, Eq, Enum)
data AutoAction = NoAuto | AutoRun deriving (Show, Eq)
data EndgameAction = NoEndgame | UnknownEndgame | Parking | Climbing | Levitate deriving (Show, Eq)
data GameData = RRR | RLR | LRL | LLL deriving (Read, Show, Eq)

instance FromJSON AutoAction where
    parseJSON = withText "alliance color" $ \s -> case s of
        "None" -> return NoAuto
        "AutoRun" -> return AutoRun
        _ -> fail $ "Expected an Auto Mode, found: " ++ ( unpack s )

instance FromJSON EndgameAction where
    parseJSON = withText "endgame action" $ \s -> case s of
        "None" -> return NoEndgame
        "Unknown" -> return UnknownEndgame
        "Parking" -> return Parking
        "Climbing" -> return Climbing
        "Levitate" -> return Levitate
        _ -> fail $ "Expected an Endgame, found: " ++ ( unpack s )

instance FromJSON GameData where
    parseJSON = withText "gameData" $ \t -> let s = unpack t in case readMaybe s of
        Just d -> return d
        Nothing -> fail $ "Expected (RRR | RLR | LRL | LLL), found: " ++ s
        