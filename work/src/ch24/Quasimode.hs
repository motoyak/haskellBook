{-# LANGUAGE QuasiQuotes #-}
module Quasimode where

import Text.RawString.QQ

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

