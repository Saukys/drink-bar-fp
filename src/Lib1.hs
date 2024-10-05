module Lib1
  ( completions,
  )
where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["Create", "Serve", "List", "Price", "Ingredients", "Liquor", "Syrup", "Juice", "Garnish", "ml", "oz", "dash", "splash"]
