{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

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
    Parser,
    Ingredients (..),
  )
where

import qualified Data.Char as C
import qualified Data.List as L

data Query = Create Drink | Serve Drink | Menu | ShowIngredients | AddIngredient Ingredient | Money
  deriving (Show, Eq)

data Currency = USD | EUR | JPY | GBP
  deriving (Show, Eq)

data Drink = Drink
  { name :: String,
    price :: Price,
    ingredients :: Ingredients
  }
  deriving (Show, Eq)

data Price = Price Double Currency
  deriving (Show, Eq)

data Ingredients = Ingredients String [Ingredient]
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

type Parser a = String -> Either String (a, String)

or6 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or6 a b c d e f = \input ->
  case a input of
    Right (v1, r1) -> Right (v1, r1)
    Left _ ->
      case b input of
        Right (v2, r2) -> Right (v2, r2)
        Left _ ->
          case c input of
            Right (v3, r3) -> Right (v3, r3)
            Left _ ->
              case d input of
                Right (v4, r4) -> Right (v4, r4)
                Left _ ->
                  case e input of
                    Right (v5, r5) -> Right (v5, r5)
                    Left _ -> f input

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h : t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

parseWord :: Parser String
parseWord [] = Left "Empty string"
parseWord str =
  let (_, rest1) = case parseWhitespaces str of
        Right (s, r) -> (s, r)
        Left _ -> ("", str)
      word = L.takeWhile C.isLetter rest1
      rest = drop (length word) rest1
   in case word of
        [] -> Left "No word found"
        _ -> Right (word, rest)

parseWhitespaces :: Parser String
parseWhitespaces [] = Right ("", [])
parseWhitespaces s@(h : t) = if C.isSpace h then Right (" ", t) else Right ("", s)

parseWord' :: String -> Parser String
parseWord' word = \input ->
  let (_, rest1) = case parseWhitespaces input of
        Right (s, r) -> (s, r)
        Left _ -> ("", input)
      (parsedWord, rest) = L.splitAt (length word) rest1
   in if parsedWord == word
        then Right (parsedWord, rest)
        else Left $ word ++ " not found in " ++ input

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' d a b c = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) -> Right (d v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (c v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

parseDouble :: Parser Double
parseDouble [] = Left "Empty string"
parseDouble str =
  let (_, rest1) = case parseWhitespaces str of
        Right (s, r) -> (s, r)
        Left _ -> ("", str)
      number = L.takeWhile C.isDigit rest1
      rest = drop (length number) rest1
      (dot, rest2) = case parseChar '.' rest of
        Right (d, r) -> (d, r)
        Left _ -> ('.', rest)
      decimal = L.takeWhile C.isDigit rest2
      rest3 = drop (length decimal) rest2
   in case number of
        [] -> Left "No number found"
        _ -> case decimal of
          [] -> Right (read number, rest2)
          _ -> Right (read $ number ++ [dot] ++ decimal, rest3)

parseCurrency :: Parser Currency
parseCurrency = \input ->
  case parseWord input of
    Right (word, rest) -> case word of
      "USD" -> Right (USD, rest)
      "EUR" -> Right (EUR, rest)
      "JPY" -> Right (JPY, rest)
      "GBP" -> Right (GBP, rest)
      _ -> Left "Currency not found"
    Left e -> Left e

parsePrice :: Parser Price
parsePrice = and2' Price parseDouble parseCurrency

parseUnit :: Parser Unit
parseUnit = \input ->
  case parseWord input of
    Right (word, rest) -> case word of
      "ml" -> Right (ML, rest)
      "oz" -> Right (OZ, rest)
      "dash" -> Right (Dash, rest)
      "splash" -> Right (Splash, rest)
      _ -> Left "Unit not found"
    Left e -> Left e

parseQuantity :: Parser Quantity
parseQuantity = and2' Quantity parseDouble parseUnit

parseIngredient :: Parser Ingredient
parseIngredient = and2' Ingredient parseQuantity parseWord

parseIngredients :: Parser Ingredients
parseIngredients = and2' Ingredients (parseWord' "ingredients: ") parseIngredientList

parseIngredientList :: Parser [Ingredient]
parseIngredientList = or2' [parseIngredient] parseIngredientList'

parseIngredientList' :: Parser [Ingredient]
parseIngredientList' = and2'' parseIngredient parseIngredientList

and2'' :: Parser a -> Parser [a] -> Parser [a]
and2'' a b = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (v1 : v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

or2' :: [Parser a] -> Parser [a] -> Parser [a]
or2' [] b = \input -> b input
or2' (a : _) b = \input ->
  case b input of
    Right (v1, r1) -> Right (v1, r1)
    Left e1 ->
      case a input of
        Right (v2, r2) -> Right ([v2], r2)
        Left e2 -> Left (e1 ++ ", " ++ e2)

parseDrink :: Parser Drink
parseDrink = and3' Drink parseWord parsePrice parseIngredients

parseCreate :: Parser Query
parseCreate = \input ->
  case parseWord' "create" input of
    Right (_, rest) -> case parseDrink rest of
      Right (drink, rest1) -> Right (Create drink, rest1)
      Left e -> Left e
    Left e -> Left e

parseServe :: Parser Query
parseServe = \input ->
  case parseWord' "serve" input of
    Right (_, rest) -> case parseDrink rest of
      Right (drink, rest1) -> Right (Serve drink, rest1)
      Left e -> Left e
    Left e -> Left e

parseMenu :: Parser Query
parseMenu = \input ->
  case parseWord' "menu" input of
    Right (_, rest) -> Right (Menu, rest)
    Left e -> Left e

parseShowIngredients :: Parser Query
parseShowIngredients = \input ->
  case parseWord' "show ingredients" input of
    Right (_, rest) -> Right (ShowIngredients, rest)
    Left e -> Left e

parseAddIngredient :: Parser Query
parseAddIngredient = \input ->
  case parseWord' "add ingredient" input of
    Right (_, rest) -> case parseIngredient rest of
      Right (ingredient, rest1) -> Right (AddIngredient ingredient, rest1)
      Left e -> Left e
    Left e -> Left e

parseMoney :: Parser Query
parseMoney = \input ->
  case parseWord' "money" input of
    Right (_, rest) -> Right (Money, rest)
    Left e -> Left e

-- User Input
parseQuery :: String -> Either String Query
parseQuery st = case or6 parseCreate parseServe parseMenu parseShowIngredients parseAddIngredient parseMoney st of
  Right (query, "") -> Right query
  Right (_, rest) -> Left $ "Unparsed: " ++ rest
  Left e -> Left "No query found"

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
    ShowIngredients -> Right (Just $ show $ inventory st, st)
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

addMoney :: Price -> State -> State
addMoney (Price p EUR) st = st {money = money st + p}
addMoney (Price p USD) st = st {money = money st + (p * 0.91)}
addMoney (Price p JPY) st = st {money = money st + (p * 0.0061)}
addMoney (Price p GBP) st = st {money = money st + (p * 1.19)}

canServe :: Drink -> State -> Bool
canServe drink st = all (\i -> i `elem` inventory st) (let (Ingredients _ ingList) = ingredients drink in ingList)

removeIngredients :: Drink -> State -> State
removeIngredients drink st = st {inventory = filter (\i -> i `notElem` ingList) (inventory st)}
  where
    (Ingredients _ ingList) = ingredients drink