{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day01.Solution
  ( day01
  , parseInput
  , input
  , phaseOne
  , phaseTwo
  ) where

import Import hiding (Left, Right)

import qualified Data.List as List

import qualified Data.Text.Encoding as TextEncoding
import qualified Data.FileEmbed as FileEmbed

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

type Amount
  = Int

data Direction
  = Left Amount
  | Right Amount
  deriving Show

parseInput :: Text -> [Direction]
parseInput input =
  input
    & Import.lines
    & fmap (Text.splitAt 1)
    & (fmap . fmap) TextRead.decimal
    & fmap sequenceA
    & Either.rights
    & (fmap . fmap) fst
    & fmap toDirection
  where
    toDirection ("L", amount) = Left amount
    toDirection ("R", amount) = Right amount
    toDirection _ = error "The direction should be either L or R"

input :: Text
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Day01/input.txt")

phaseOne :: Text -> Int
phaseOne input =
  let
    startingDestination =
      50

    distances =
      parseInput input

    destinations =
      List.foldl turn [startingDestination] distances

    turn destinations@(currentDestination:rest) (Left amount) =
      if currentDestination - amount > 0 then
        (currentDestination - amount:destinations)
      else
        loopLeft (currentDestination - amount)

      where
        loopLeft destination =
          if destination + 100 < 0 then
            loopLeft (destination + 100)
          else if destination == 0 then
            (destination:destinations)
          else
            (destination+100:destinations)

    turn destinations@(currentDestination:rest) (Right amount) =
      if currentDestination + amount < 99 then
        (currentDestination + amount:destinations)
      else
        loopRight (currentDestination + amount)

      where
        loopRight destination =
          if destination - 100 > 99 then
            loopRight (destination - 100)
          else
            (destination-100:destinations)

    zeros =
      foldr (\destination sofar -> if destination == 0 then sofar + 1 else sofar) 0 destinations
  in
    zeros

-- phaseTwo :: Text -> Int
phaseTwo :: Text -> (Int, [Int])
phaseTwo input =
  let
    startingDestination =
      50

    distances =
      parseInput input

    (iterations, destinations) =
      List.foldl turn (0, [startingDestination]) distances

    turn :: (Int, [Int]) -> Direction -> (Int, [Int])
    turn (iterations, destinations@(currentDestination:rest)) (Left amount) =
      if currentDestination - amount > 0 then
        (iterations, currentDestination - amount : destinations)
      else
        loopLeft (currentDestination - amount) iterations

      where
        loopLeft :: Int -> Int -> (Int, [Int])
        loopLeft destination iterations =
          if destination + 100 < 0 then
            loopLeft (destination + 100) (iterations + 1)
          else if destination == 0 then
            (iterations, destination : destinations)
          else
            (iterations, destination + 100 : destinations)

    turn (iterations, destinations@(currentDestination:rest)) (Right amount) =
      if currentDestination + amount <= 100 then
        if currentDestination + amount == 100 then
          (iterations, 0:destinations)
        else
          (iterations, currentDestination + amount:destinations)
      else
        loopRight (currentDestination + amount) iterations

      where
        loopRight :: Int -> Int -> (Int, [Int])
        loopRight destination iterations =
          if destination - 100 > 99 then
            loopRight (destination - 100) (iterations + 1)
          else
            (iterations + 1, destination - 100 : destinations)

    zeros =
      foldr (\destination sofar -> if destination == 0 then sofar + 1 else sofar) 0 destinations
  in
    (iterations, List.reverse destinations)
    -- zeros

day01 :: IO ()
day01 = do
  putStrLn "Day01 Phase 1"
  let someVariable = phaseOne input --
  putStrLn $ "Some Variable: " ++ tshow someVariable

  putStrLn "Day01 Phase 2"
  let someOtherVariable = phaseTwo input --
  putStrLn $ "Some Other Variable: " ++ tshow someOtherVariable
