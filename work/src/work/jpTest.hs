module JpTest where

newtype T個数 a = T個数 Integer
newtype T単価 a = T単価 Integer
newtype T合計金額 a = T合計金額 Integer deriving (Show)

合計金額を求める式 :: T個数 Integer -> T単価 Integer -> T合計金額 Integer
合計金額を求める式 (T個数 x) (T単価 y) = T合計金額 (x * y)

リンゴ個数 = T個数 3
リンゴ単価 = T単価 128

ans結果表示 = 合計金額を求める式 リンゴ個数 リンゴ単価


class Kakerareru a where
  toNumberKakerareru :: a -> Integer

class Kakeru a where
  toNumberKakeru :: a -> Integer

newtype Kosuu a = Kosuu Integer
newtype Tanka a = Tanka Integer
newtype Goukei a = Goukei Integer deriving (Show)

instance Kakerareru (Tanka a) where
  toNumberKakerareru (Tanka x) = x

instance Kakeru (Kosuu a) where
  toNumberKakeru (Kosuu x) = x


siki :: (Kakeru a, Kakerareru b) => a -> b -> Goukei Integer
siki x y = Goukei $ (toNumberKakeru x) * (toNumberKakerareru y)

appleKosuu = Kosuu 3
appleUnitPrice = Tanka 128

infixl 5 *****
(*****) :: (Kakeru a, Kakerareru b) => a -> b -> Goukei Integer
(*****) = siki
