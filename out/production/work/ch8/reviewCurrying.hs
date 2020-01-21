module ReviewCurrying where

  cattyConny :: String -> String -> String
  cattyConny x y = x ++ " mrow " ++ y

  flippy :: String -> String -> String
  flippy = flip cattyConny

  appedCatty :: String -> String
  appedCatty = cattyConny "woops"

  frappe :: String -> String
  frappe = flippy "haha"

  q1 = appedCatty "woohoo!"
  q2 = frappe "1"
  q3 = frappe (appedCatty "2")
  q4 = appedCatty (frappe "blue")
  q5 = cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
  q6 = cattyConny (flippy "Pugs" "are") "awesome"
