{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TooMany where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (>42)

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
