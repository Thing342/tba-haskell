{-# LANGUAGE OverloadedStrings #-}

module TBA.Enums where

import Data.Aeson
import Data.Aeson.Types

import qualified Data.Text as T

data CompetitionLevel = Qualification | Eighthfinals | Quarterfinals | Semifinals | Finals deriving (Show, Eq, Ord, Enum)

instance FromJSON CompetitionLevel where
    parseJSON = withText "competition level" $ \s -> case s of
        "qm" -> return Qualification
        "ef" -> return Eighthfinals
        "qf" -> return Quarterfinals
        "sf" -> return Semifinals
        "f"  -> return Finals
        _ -> fail $ "invalid qualification value: " ++ (T.unpack s)

data MatchWinner = RedWin | BlueWin | Tie deriving (Show, Eq)
instance FromJSON MatchWinner where
    parseJSON = withText "alliance color" $ \s -> case s of
        "red" -> return RedWin
        "blue" -> return BlueWin
        "" -> return Tie 
        _ -> fail $ "invalid alliance color: '" ++ (T.unpack s) ++ "'"

data AllianceColor = Red | Blue deriving (Show, Eq, Ord, Enum)
instance FromJSON AllianceColor where
    parseJSON = withText "alliance color" $ \s -> case s of
        "red" -> return Red
        "blue" -> return Blue
        _ -> fail $ "invalid alliance color: '" ++ (T.unpack s) ++ "'"