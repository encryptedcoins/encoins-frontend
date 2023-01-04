{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Reflex.Dom

import           ENCOINS.Body (bodyWidget)
import           ENCOINS.Head (headWidget)

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget