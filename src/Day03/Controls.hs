{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day03.Controls where

import Import

import qualified Data.Text.Encoding as TextEncoding
import qualified Data.FileEmbed as FileEmbed

input :: Text
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Day03/control_input.txt")

part1Solution :: Int
part1Solution = 357

someOtherVariable :: Int
someOtherVariable = error "do this"
