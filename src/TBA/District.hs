{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TBA.District where

data District = CHS | MI | FMA | NC | IN | ISR | NE | ONT | PCH | PNW | TX deriving (Eq, Show)

districtName :: Int -> District -> String
districtName _ CHS = "chs"
districtName _ MI = "fim"
districtName year FMA = if year >= 2019 then "fma" else "mar"
districtName year NC = if year >= 2019 then "fnc" else "nc"
districtName _ IN = "in"
districtName _ ISR = "isr"
districtName _ NE = "ne"
districtName _ ONT = "ont"
districtName _ PCH = "pch"
districtName _ PNW = "pnw"
districtName _ TX = "tx"

districtCode :: Int -> District -> String
districtCode year district = (show year) ++ (districtName year district)