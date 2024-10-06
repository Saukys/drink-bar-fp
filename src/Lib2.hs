{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
    Drink (..),
    Price (..),
    Ingredient (..),
    Quantity (..),
    Currency (..),
    Unit (..),
    parseCurrency,
    parseUnit,
    parseQuantity,
    parsePrice,
    parseIngredient,
    parseIngredients,
    parseDrink,
    findDrink,
  )
where

data Query = Create Drink | Serve Drink | Menu | Ingredients | AddIngredient Ingredient | Money
  deriving (Show, Eq)

data Currency = USD | EUR | JPY | GBP
  deriving (Show, Eq)

data Drink = Drink
  { name :: String,
    price :: Price,
    ingredients :: [Ingredient]
  }
  deriving (Show, Eq)

data Price = Price Double Currency
  deriving (Show, Eq)

data Unit = ML | OZ | Dash | Splash
  deriving (Show, Eq)

data Quantity = Quantity Double Unit
  deriving (Show, Eq)

data Ingredient = Ingredient Quantity String
  deriving (Show, Eq)

data State = State
  { money :: Double, -- in EUR
    inventory :: [Ingredient],
    menu :: [Drink]
  }
  deriving (Show, Eq)

-- <currency> ::= "USD" | "EUR" | "GBP" | "JPY"
parseCurrency :: String -> Either String Currency
parseCurrency "USD" = Right USD
parseCurrency "EUR" = Right EUR
parseCurrency "JPY" = Right JPY
parseCurrency "GBP" = Right GBP
parseCurrency _ = Left "Invalid currency"

-- <unit> ::= "ml" | "oz" | "dash" | "splash"
parseUnit :: String -> Either String Unit
parseUnit "ml" = Right ML
parseUnit "oz" = Right OZ
parseUnit "dash" = Right Dash
parseUnit "splash" = Right Splash
parseUnit _ = Left "Invalid unit"

-- <quantity> ::= <number> <unit>
parseQuantity :: String -> Either String Quantity
parseQuantity s = case words s of
  [q, u] -> case reads q of
    [(q', _)] -> case parseUnit u of
      Right u' -> Right $ Quantity q' u'
      Left e -> Left e
    _ -> Left "Invalid quantity"
  _ -> Left "Invalid quantity"

-- <Price> ::= <number> <currency>
parsePrice :: String -> Either String Price
parsePrice s =
  let (v, c) = span (`elem` "0123456789.") s
   in case reads v of
        [(v', _)] -> case parseCurrency c of
          Right c' -> Right $ Price v' c'
          Left e -> Left e
        _ -> Left $ "Invalid price: " ++ v

-- <Ingredient> ::= <quantity> <name>
parseIngredient :: String -> Either String Ingredient
parseIngredient s = case words s of
  q1 : q2 : name -> case parseQuantity (unwords [q1, q2]) of
    Right q' -> case name of
      [] -> Left "Invalid ingredient"
      _ -> Right $ Ingredient q' (unwords name)
    Left e -> Left e
  _ -> Left $ "Invalid ingredient : " ++ s

-- <ingredient-list> ::= <ingredient> | <ingredient> <ingredient-list>
parseIngredientList :: String -> Either String [Ingredient]
parseIngredientList s =
  let ingredients = splitOn "," s
   in mapM parseIngredient ingredients

-- <Ingredients> ::= "Ingredients" <ingredient-list>
parseIngredients :: [String] -> Either String [Ingredient]
parseIngredients s = case s of
  "Ingredients:" : is -> parseIngredientList (unwords is)
  _ -> Left $ "Invalid ingredients :" ++ unwords s

-- <Drink> ::= <name> <price> <ingredient-list>
parseDrink :: String -> Either String Drink
parseDrink s = case words s of
  name : price : ingredients -> case parsePrice price of
    Right price' -> case parseIngredients ingredients of
      Right ingredients' -> Right $ Drink name price' ingredients'
      Left e -> Left e
    Left e -> Left e
  _ -> Left "Invalid drink"

-- User Input
parseQuery :: String -> Either String Query
parseQuery s = case words s of
  -- <create> ::= "Create" <drink>
  "Create" : drink -> case parseDrink (unwords drink) of
    Right drink' -> Right $ Create drink'
    Left e -> Left e
  -- <serve> ::= "Serve" <drink>
  "Serve" : drink -> case parseDrink (unwords drink) of
    Right drink' -> Right $ Serve drink'
    Left e -> Left e
  "Menu" : [] -> Right Menu
  "Ingredients" : [] -> Right Ingredients
  -- <add-ingredient> ::= "AddIngredient" <ingredient>
  "AddIngredient" : ingredient -> case parseIngredient (unwords ingredient) of
    Right ingredient' -> Right $ AddIngredient ingredient'
    Left e -> Left e
  _ -> Left "Invalid query"

emptyState :: State
emptyState = State {money = 0, inventory = [], menu = []}

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st eitherQuery = case eitherQuery of
  query -> case query of
    Menu -> Right (Just $ show $ menu st, st)
    Ingredients -> Right (Just $ show $ inventory st, st)
    Create drink -> Right (Nothing, st {menu = drink : menu st})
    Serve drink -> case findDrink drink (menu st) of
      Just d ->
        if canServe d st
          then Right (Just $ "Serving " ++ name d, removeIngredients d (addMoney (price d) st))
          else Left $ "no ingredients"
      Nothing -> Left "Drink not found"
    AddIngredient ingredient -> Right (Nothing, st {inventory = ingredient : inventory st})
    Money -> Right (Just $ show $ money st, st)

findDrink :: Drink -> [Drink] -> Maybe Drink
findDrink _ [] = Nothing
findDrink drink (h : t) = if name drink == name h then Just h else findDrink drink t

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str =
  let (before, remainder) = span (/= head delim) str
   in before : case remainder of
        [] -> []
        x -> splitOn delim (drop 1 x)

addMoney :: Price -> State -> State
addMoney (Price p EUR) st = st {money = money st + p}
addMoney (Price p USD) st = st {money = money st + (p * 0.91)}
addMoney (Price p JPY) st = st {money = money st + (p * 0.0061)}
addMoney (Price p GBP) st = st {money = money st + (p * 1.19)}

canServe :: Drink -> State -> Bool
canServe drink st = all (`elem` inventory st) (ingredients drink)

removeIngredients :: Drink -> State -> State
removeIngredients drink st = st {inventory = filter (\i -> i `notElem` ingredients drink) (inventory st)}