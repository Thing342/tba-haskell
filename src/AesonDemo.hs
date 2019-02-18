{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AesonDemo where

import Lib

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types

import Data.Traversable (for)
import Text.Read (readMaybe)
import Control.Monad (when, guard)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Foldable as F

parseTuple :: Value -> Parser (String, Bool)
parseTuple (Object obj) = do
    -- Parse "a".
    a <- obj .: "a"

    -- Parse "b".
    b <- obj .: "b"

    return (a, b)
parseTuple _          = fail "expected an object."

parseArray :: Value -> Parser[(String, Bool)]
parseArray = withArray "array of tuples" $ \arr ->
    mapM parseTuple (V.toList arr)

data Person = Person {name :: String, age :: Int} deriving Show

instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        name <- o .:  "name" <|> o .: "fullname"
        age  <- complexAgeParsing o
        return Person{..}

instance ToJSON Person where
    toJSON Person{..} = object [
        "name" .= name,
        "age"  .= age ]

complexAgeParsing :: Object -> Parser Int
complexAgeParsing o = F.asum [
    -- Simple case: "age": number
    o .: "age",

    -- Alternative case: "age": string
    do s <- o .: "age"
       case readMaybe s of
          Nothing -> fail "Can't parse string field 'age' as a number."
          Just x -> return x,
    
    -- Weird tuple case: "AGE":[number, number]
    fst <$> (o .: "AGE" :: Parser (Int, Int))]

{-- Model Data:
{
    "website1.com": {
        "/page1": 3,
        "/page2": 4
    },
    "website2.com": {
        "/page": 10
    }
}
--}
data Referer = Referer {
    domain :: String,
    pathAccesses :: [ (String, Int) ]
} deriving (Show)

parseReferers :: Value -> Parser [Referer]
parseReferers p =
    map (\(domain, accesses) -> Referer domain (HM.toList accesses)) .
    HM.toList <$>
    parseJSON p

parseReferers' :: Value -> Parser [Referer]
parseReferers' = withObject "referers" $ \o ->
    for (HM.toList o) $ \(domain, referer) -> do
        accesses <- HM.toList <$> parseJSON referer
        let accesses' = map (\(page, n) -> (T.unpack page, n)) accesses
        return $ Referer {
            domain = T.unpack domain,
            pathAccesses = accesses'
        }