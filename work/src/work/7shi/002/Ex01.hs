module Ex01 where

data Color =
    Red
  | Green
  | Blue
  | Cyan
  | Magenta
  | Yellow
  | White
  deriving (Eq, Show, Enum)

mix :: Color -> Color -> Color
mix Red Green = Yellow
mix Red Blue = Magenta
mix Red Cyan = White
mix Red Magenta = Magenta
mix Red Yellow = Yellow
mix Green Blue = Cyan
mix Green Cyan = Cyan
mix Green Magenta = White
mix Green Yellow = Yellow
mix Blue Cyan = Cyan
mix Blue Magenta = Magenta
mix Blue Yellow = White
mix Cyan Magenta = White
mix Cyan Yellow = White
mix Magenta Yellow = White
mix White _ = White
mix c1 c2
  | c1 == c2 = c1
  | otherwise = mix c2 c1
