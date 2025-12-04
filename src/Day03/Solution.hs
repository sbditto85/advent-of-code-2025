{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day03.Solution
  ( day03
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

parseInput :: Text -> [[Int]]
parseInput input =
  input
    & Import.lines
    & fmap (Text.chunksOf 1)
    & (fmap . fmap) TextRead.decimal
    & fmap sequenceA
    & Either.rights
    & (fmap . fmap) fst

input :: Text
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Day03/input.txt")

phaseOne :: Text -> Int
phaseOne input =
  let
    batteryBanks =
      parseInput input

    largestJoltages =
      fmap largestJoltage batteryBanks
  in
    sum largestJoltages

largestJoltage :: [Int] -> Int
largestJoltage batteryBank =
  let
    largestBattery =
       maximum $ impureNonNull $ (reverse . drop 1 . reverse) batteryBank

    biggestRightMostBattery =
      search largestBattery (-1) batteryBank
  in
    largestBattery * 10 + biggestRightMostBattery

  where
    search _largestBattery biggestRightMostBattery [] =
      biggestRightMostBattery
    search largestBattery biggestRightMostBattery (battery:batteryBank) =
      if largestBattery == battery then
        let
          currentBiggestRightMostBattery =
            biggest biggestRightMostBattery batteryBank
        in
          search largestBattery currentBiggestRightMostBattery batteryBank
      else
        search largestBattery biggestRightMostBattery batteryBank

    biggest currentBiggest [] =
      currentBiggest
    biggest currentBiggest (battery:batteryBank) =
      if battery > currentBiggest then
        biggest battery batteryBank
      else
        biggest currentBiggest batteryBank

phaseTwo :: Text -> Int
phaseTwo input =
  error "do this"

day03 :: IO ()
day03 = do
  putStrLn "Day03 Phase 1"
  let sumOfBatteries = phaseOne input -- 17179
  putStrLn $ "Sum of batteries: " ++ tshow sumOfBatteries

  putStrLn "Day03 Phase 2"
  let someOtherVariable = phaseTwo input --
  putStrLn $ "Some Other Variable: " ++ tshow someOtherVariable
