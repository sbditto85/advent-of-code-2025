{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Template.Solution
  ( template
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
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Template/input.txt")

phaseOne :: Text -> Int
phaseOne input =
  error "do this"

phaseTwo :: Text -> Int
phaseTwo input =
  error "do this"

template :: IO ()
template = do
  putStrLn "Template Phase 1"
  let someVariable = phaseOne input --
  putStrLn $ "Some Variable: " ++ tshow someVariable

  putStrLn "Template Phase 2"
  let someOtherVariable = phaseTwo input --
  putStrLn $ "Some Other Variable: " ++ tshow someOtherVariable
