module JpTest where

type Kata個数 = Integer
type Kata単価 = Integer
type Kata合計金額 = Integer

合計金額を求める式 :: Kata個数 -> Kata単価 -> Kata合計金額
合計金額を求める式 個数 単価 = 個数 * 単価

リンゴ個数 = 3 :: Integer
リンゴ単価 = 128 :: Integer

ans結果表示 = 合計金額を求める式 リンゴ個数 リンゴ単価
