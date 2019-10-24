module RegisteredUser where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User =
    UnregisterdUser
  | RegisteredUser Username AccountNumber
