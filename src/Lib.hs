{-# LANGUAGE OverloadedStrings #-}

module Lib where

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = let 
    acc = (\next (sum, count) -> (sum + next, count + 1))
    (sum, count) = foldr acc (0,0) xs 
    in sum / count