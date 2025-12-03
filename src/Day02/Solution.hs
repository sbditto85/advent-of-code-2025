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
  input
    & Text.strip
    & Text.splitOn ","
    & fmap (Text.breakOn "-")
    & (fmap . fmap) (Text.drop 1)
    & fmap (\(start, end) ->
             (fst . fromRight' . TextRead.decimal $ start, fst . fromRight' . TextRead.decimal $ end)
           )

  where
    fromRight' =
      Either.fromRight (error "Could not parse input")

input :: Text
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Day02/input.txt")

phaseOne :: Text -> Int
phaseOne input =
  let
    ranges =
      parseInput input

    invalidNumbers = do
      range <- ranges
      findInvalid (fst range) (snd range)
  in
    sum invalidNumbers

findInvalid :: Int -> Int -> [Int]
findInvalid start end =
  find start end []
  where

    find :: Int -> Int -> [Int] -> [Int]
    find start end soFar =
      if start > end then
        soFar
      else
        let
          soFar' =
            if isInvalid start then
              start : soFar
            else
              soFar
        in
          find (start + 1) end soFar'

      where
        isInvalid number =
          let
            text =
              Text.show number

            textLength =
              Text.length text

            middle =
              textLength `div` 2

            (firstHalf, secondHalf) =
              Text.splitAt middle text
          in
            if textLength `mod` 2 /= 0 then
              False
            else
              firstHalf == secondHalf

phaseTwo :: Text -> Int
phaseTwo input =
  error "do this"

day02 :: IO ()
day02 = do
  putStrLn "Day02 Phase 1"
  let someVariable = phaseOne input -- 13919717792
  putStrLn $ "Invalid numbers: " ++ tshow someVariable

  putStrLn "Day02 Phase 2"
  let someOtherVariable = phaseTwo input --
  putStrLn $ "Some Other Variable: " ++ tshow someOtherVariable
