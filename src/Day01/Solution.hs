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
phaseTwo :: Text -> [Int]
phaseTwo input =
  let
    startingDestination =
      50

    distances =
      parseInput input

    destinationsAndZeros =
      List.foldl turn [startingDestination] distances

    turn destinations@(currentDestination:rest) (Left amount) =
      if currentDestination - amount >= 0 then
        (currentDestination - amount) : destinations
      else
        let
          startingZeros =
            []

          (destination, zeros) =
            loopLeft (currentDestination - amount) startingZeros

          zeros' =
            if currentDestination == 0 then
              drop 1 zeros
            else
              zeros
        in
          (destination : zeros') ++ destinations

      where
        loopLeft :: Int -> [Int] -> (Int, [Int])
        loopLeft destination zeros =
          if destination >= (-100) then
            if destination == 0 || destination == (-100) then
              (0, zeros)
            else
              (100 + destination, 0:zeros)
          else
            loopLeft (destination + 100) (0:zeros)

    turn destinations@(currentDestination:rest) (Right amount) =
      if currentDestination + amount < 100 then
        (currentDestination + amount) : destinations
      else
        let
          startingZeros =
            []

          (destination, zeros) =
            loopRight (currentDestination + amount) startingZeros

          zeros' =
            if currentDestination == 0 then
              drop 1 zeros
            else
              zeros
        in
          (destination : zeros') ++ destinations

      where
        loopRight :: Int -> [Int] -> (Int, [Int])
        loopRight destination zeros =
          if destination <= 100 then
            if destination == 0 || destination == 100 then
              (0, zeros)
            else
              (destination, zeros)
          else
            loopRight (destination - 100) (0:zeros)

    -- turn destinations@(currentDestination:rest) (Left amount) =
    --   if currentDestination - amount > 0 then
    --     (currentDestination - amount) : destinations
    --   else
    --     let
    --       startingZeros =
    --         if currentDestination == 0 then
    --           []
    --         else
    --           [0]

    --       (destination, zeros) =
    --         loopLeft (currentDestination - amount) startingZeros
    --     in
    --       (destination : zeros) ++ destinations

    --   where
    --     loopLeft :: Int -> [Int] -> (Int, [Int])
    --     loopLeft destination zeros =
    --       if destination + 100 < 0 then
    --         loopLeft (destination + 100) (0:zeros)
    --       else if destination == 0 then
    --         (destination, drop 1 zeros)
    --       else
    --         (destination+100, zeros)

    -- turn destinations@(currentDestination:rest) (Right amount) =
    --   if currentDestination + amount < 99 then
    --     (currentDestination + amount:destinations)
    --   else
    --     let
    --       startingZeros =
    --         if currentDestination == 0 then
    --           [0]
    --         else
    --           [0]

    --       (destination, zeros) =
    --         loopRight (currentDestination + amount) startingZeros
    --     in
    --       (destination : zeros) ++ destinations

    --   where
    --     loopRight :: Int -> [Int] -> (Int, [Int])
    --     loopRight destination zeros =
    --       if destination - 100 > 99 then
    --         loopRight (destination - 100) (0:zeros)
    --       else if destination == 0 then
    --         (0, drop 1 zeros)
    --       else if destination == 100 then
    --         (0, drop 1 zeros)
    --       else
    --         (destination-100, zeros)

    zeros =
      foldr (\destination sofar -> if destination == 0 then sofar + 1 else sofar) 0 destinationsAndZeros
  in
    List.reverse destinationsAndZeros
    -- zeros

day01 :: IO ()
day01 = do
  putStrLn "Day01 Phase 1"
  let someVariable = phaseOne input --
  putStrLn $ "Some Variable: " ++ tshow someVariable

  putStrLn "Day01 Phase 2"
  let someOtherVariable = phaseTwo input --
  putStrLn $ "Some Other Variable: " ++ tshow someOtherVariable
