module Person where

type Name = String
type Age = Integer

type ValidatePerson a =
  Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True  -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

nameOkay' :: Integer -> Either [PersonInvalid] Integer
nameOkay' name = case show name /= "" of
  True -> Right name
  False -> Left [NameEmpty]


mkPerson :: Name
         -> Age
         -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left nameBad) (Left ageBad) = Left (nameBad ++ ageBad)
mkPerson' (Left nameBad) _ = Left nameBad
mkPerson' _ (Left ageBad) = Left ageBad

-- mkPerson name age
  -- | name /= "" && age >= 0 =
      -- Right $ Person name age
  -- | name == "" = Left NameEmpty
  -- | otherwise = Left AgeTooLow


-- mkPerson2 :: Name -> Age -> Validation [PersonInvalid] Person
-- mkPerson2 name age = liftA2 Person (nameOkay name) (ageOkay age)
