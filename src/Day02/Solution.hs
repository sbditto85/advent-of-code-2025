{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day02.Solution
  ( day02
  , parseInput
  , input
  , phaseOne
  , phaseTwo
  ) where

import Import

import qualified Data.Text.Encoding as TextEncoding
import qualified Data.FileEmbed as FileEmbed

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

parseInput :: Text -> [(Int, Int)]
parseInput input =
  error "do this"

input :: Text
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Day02/input.txt")

phaseOne :: Text -> Int
phaseOne input =
  error "do this"

phaseTwo :: Text -> Int
phaseTwo input =
  error "do this"

day02 :: IO ()
day02 = do
  putStrLn "Day02 Phase 1"
  let someVariable = phaseOne input --
  putStrLn $ "Some Variable: " ++ tshow someVariable

  putStrLn "Day02 Phase 2"
  let someOtherVariable = phaseTwo input --
  putStrLn $ "Some Other Variable: " ++ tshow someOtherVariable
