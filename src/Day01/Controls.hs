{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day01.Controls where

import Import

import qualified Data.Text.Encoding as TextEncoding
import qualified Data.FileEmbed as FileEmbed

input :: Text
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Day01/control_input.txt")

solution :: Int
solution = 3
