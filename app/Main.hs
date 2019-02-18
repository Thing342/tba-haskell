{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import AesonDemo

import Data.Aeson
import Data.Aeson.Types (parseEither)

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do
    putStrLn "Enter your JSON to decode below:"
    s <- T.encodeUtf8 <$> T.getLine
    let res = (parseEither parseReferers =<< eitherDecode s) :: Either String [Referer]
    putStrLn $ show res

