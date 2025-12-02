{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Template.Controls where

import Import

import qualified Data.Text.Encoding as TextEncoding
import qualified Data.FileEmbed as FileEmbed

input :: Text
input = TextEncoding.decodeUtf8Lenient $(FileEmbed.embedFileRelative "src/Template/control_input.txt")

someVariable :: Int
someVariable = error "do this"

someOtherVariable :: Int
someOtherVariable = error "do this"
