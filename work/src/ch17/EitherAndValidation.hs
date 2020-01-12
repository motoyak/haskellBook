module EitherAndValidation where

data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)

validationToEither :: Validation e a
                   -> Either e a
validationToEither (Failure err) = Left err
validationToEither (Success a) = Right a

eitherToValidation :: Either e a
                   -> Validation e a
eitherToValidation (Left err) = Failure err
eitherToValidation (Right a) = Success a
