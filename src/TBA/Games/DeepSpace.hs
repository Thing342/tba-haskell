{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TBA.Games.DeepSpace where

import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.EnumMap (EnumMap, fromList)
import GHC.Enum

import Data.Traversable (for)

data BayState = NoBay | Panel | Cargo | PanelAndCargo deriving (Show, Eq, Ord, Enum)
data HabLine = NotCrossed | CrossedTeleop | CrossedSandstorm deriving (Show, Eq, Ord, Enum)
data HabLevel = NoHab | HabLevel1 | HabLevel2 | HabLevel3 deriving (Show, Eq, Ord, Enum)

parseBayState :: Value -> Parser (Maybe BayState)
parseBayState (String v) = case (toLower v) of
    "none" -> return $ Just NoBay
    "panel" -> return $ Just Panel
    "cargo" -> return $ Just Cargo
    "panelandcargo" -> return $ Just PanelAndCargo
    "unknown" -> return $ Nothing
    _ -> fail "Not a valid bay state."
parseBayState _ = fail "Expected String."

parseHabLine :: Value -> Parser (Maybe HabLine)
parseHabLine (String v) = case (toLower v) of
    "crossedhablineinsandstorm" -> return $ Just CrossedSandstorm
    "crossedhablineinteleop" -> return $ Just CrossedTeleop
    "none" -> return $ Just NotCrossed
    "unknown" -> return Nothing
    _ -> fail "Not a valid HabLine state."
parseHabLine _ = fail "Expected String"

parseHabLevel :: Value -> Parser (Maybe HabLevel)
parseHabLevel (String v) = case (toLower v) of
    "none" -> return $ Just NoHab
    "hablevel1" -> return $ Just HabLevel1
    "hablevel2" -> return $ Just HabLevel2
    "hablevel3" -> return $ Just HabLevel3
    "unknown" -> return Nothing
    _ -> fail "Not a valid HabLevel."
parseHabLevel _ = fail "Expected a String."

data RobotPos = Robot1 | Robot2 | Robot3 deriving (Show, Eq, Enum)
data BayPos = Bay1 | Bay2 | Bay3 | Bay4 | Bay5 | Bay6 | Bay7 | Bay8 deriving (Show, Eq, Enum, Bounded)
data RocketSide = RocketNear | RocketFar deriving (Show, Eq, Enum)
data RocketHeight = RocketLow | RocketMid | RocketHigh deriving (Show, Eq, Enum)
data RocketFace = RocketLeft | RocketRight deriving (Show, Eq, Enum)

preMatchBayKey :: BayPos -> Text
preMatchBayKey b = pack ("preMatch" ++ show b)

bayKey :: BayPos -> Text
bayKey = toLower . pack . show

rocketSideStr :: RocketSide -> String
rocketSideStr RocketNear = "Near"
rocketSideStr RocketFar = "Far"

rocketHeightStr :: RocketHeight -> String
rocketHeightStr RocketLow = "low"
rocketHeightStr RocketMid = "mid"
rocketHeightStr RocketHigh = "top"

rocketFaceStr :: RocketFace -> String
rocketFaceStr RocketLeft = "Left"
rocketFaceStr RocketRight = "Right"

completedRocketKey :: RocketSide -> Text
completedRocketKey side = pack ( "completedRocket" ++ (rocketSideStr side ))

data RocketPos = RocketPos RocketSide RocketHeight RocketFace deriving (Show, Eq)
instance Enum RocketPos where
    toEnum i = let 
        (rest, side) = (i `div` 2, toEnum $ i `mod` 2)
        (rest', height) = (rest `div` 3, toEnum $ rest `mod` 3)
        face = toEnum $ rest' `mod` 2
        in RocketPos side height face
    
    fromEnum (RocketPos RocketNear RocketLow RocketLeft) = 0
    fromEnum (RocketPos RocketFar RocketLow RocketLeft) = 1
    fromEnum (RocketPos RocketNear RocketMid RocketLeft) = 2
    fromEnum (RocketPos RocketFar RocketMid RocketLeft) = 3
    fromEnum (RocketPos RocketNear RocketHigh RocketLeft) = 4
    fromEnum (RocketPos RocketFar RocketHigh RocketLeft) = 5
    fromEnum (RocketPos RocketNear RocketLow RocketRight) = 6
    fromEnum (RocketPos RocketFar RocketLow RocketRight) = 7
    fromEnum (RocketPos RocketNear RocketMid RocketRight) = 8
    fromEnum (RocketPos RocketFar RocketMid RocketRight) = 9
    fromEnum (RocketPos RocketNear RocketHigh RocketRight) = 10
    fromEnum (RocketPos RocketFar RocketHigh RocketRight) = 11

    enumFrom = boundedEnumFrom

instance Bounded RocketPos where
    minBound = RocketPos RocketNear RocketLow RocketLeft
    maxBound = RocketPos RocketFar RocketHigh RocketRight

rocketPosKey :: RocketPos -> Text
rocketPosKey (RocketPos s h f) = 
    let v = (rocketHeightStr h) ++ (rocketFaceStr f) ++ "Rocket" ++ (rocketSideStr s) 
    in pack v

data GamePiecePos = Rocket RocketPos | Bay BayPos deriving (Show, Eq)
instance Enum GamePiecePos where
    toEnum i | i < 8  = Bay $ toEnum i
             | i >= 8 = Rocket $ toEnum (i-8)

    fromEnum (Rocket r) = 8 + fromEnum r
    fromEnum (Bay b)    = fromEnum b

    enumFrom = boundedEnumFrom

instance Bounded GamePiecePos where
    minBound = Bay minBound
    maxBound = Rocket maxBound

gamePiecePosKey :: GamePiecePos -> Text
gamePiecePosKey (Bay b) = bayKey b
gamePiecePosKey (Rocket r) = rocketPosKey r

preMatchBayParse :: Object -> Parser (EnumMap BayPos (Maybe BayState))
preMatchBayParse o = let
    mapper Bay4 = return (Bay4, Just Cargo)
    mapper Bay5 = return (Bay5, Just Cargo)
    mapper enum = do
        let key = preMatchBayKey enum
        val <- o .: key
        sub <- parseBayState val
        return (enum, sub)
    in (liftM fromList) ( mapM mapper [Bay1 ..] )