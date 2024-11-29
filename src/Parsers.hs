{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsers
  ( Query (..),
    Currency (..),
    Drink (..),
    Price (..),
    Ingredients (..),
    Unit (..),
    Quantity (..),
    Ingredient (..),
    Parser (..),
    parseTaskList,
    parseTask,
    parseCreate,
    parseServe,
    parseMenu,
    parseShowIngredients,
    parseAddIngredient,
    parseMoney,
    parseDebug,
    skipSpaces,
    parseLiteral,
    parseString,
    parseInt,
    parseChar,
    char,
  )
where

import Control.Applicative (Alternative (empty), optional, (<|>))
import Data.Char (isDigit)

data Query = Create Drink | Serve Drink | Menu | ShowIngredients | AddIngredient Ingredient | Money | Debug | MoneyAdd Price | Sequence [Query]
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

newtype Parser a = Parser {parse :: String -> Either String (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser $ \str -> case parse pf str of
    Left e -> Left e
    Right (f, r) -> parse (f <$> pa) r

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = Parser $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = Parser $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (char ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

parseTask :: Parser Query
parseTask =
  parseCreate
    <|> parseServe
    <|> parseMenu
    <|> parseShowIngredients
    <|> parseAddIngredient
    <|> parseMoney
    <|> parseDebug
    <|> parseMoneyAdd

parseMoneyAdd :: Parser Query
parseMoneyAdd = do
  _ <- parseLiteral "add_money"
  _ <- parseLiteral "("
  p <- parsePrice
  _ <- parseLiteral ")"
  return $ MoneyAdd p

parseCreate :: Parser Query
parseCreate = do
  _ <- parseLiteral "create"
  _ <- parseLiteral "("
  drink <- parseDrink
  _ <- parseLiteral ")"
  return $ Create drink

parseServe :: Parser Query
parseServe = do
  _ <- parseLiteral "serve"
  _ <- parseLiteral "("
  drink <- parseDrink
  _ <- parseLiteral ")"
  return $ Serve drink

parseMenu :: Parser Query
parseMenu = do
  _ <- parseLiteral "menu"
  _ <- parseLiteral "("
  _ <- parseLiteral ")"
  return Menu

parseShowIngredients :: Parser Query
parseShowIngredients = do
  _ <- parseLiteral "show_ingredients"
  _ <- parseLiteral "("
  _ <- parseLiteral ")"
  return ShowIngredients

parseAddIngredient :: Parser Query
parseAddIngredient = do
  _ <- parseLiteral "add"
  _ <- parseLiteral "("
  ingredient <- parseIngredient
  _ <- parseLiteral ")"
  return $ AddIngredient ingredient

parseMoney :: Parser Query
parseMoney = do
  _ <- parseLiteral "money"
  _ <- parseLiteral "("
  _ <- parseLiteral ")"
  return Money

parseDebug :: Parser Query
parseDebug = do
  _ <- parseLiteral "debug"
  _ <- parseLiteral "("
  _ <- parseLiteral ")"
  return Debug

parseDrink :: Parser Drink
parseDrink = do
  name <- parseString
  price <- parsePrice
  ingredients <- parseIngredients
  return $ Drink name price ingredients

parsePrice :: Parser Price
parsePrice = do
  amount <- parseFloat
  currency <- parseCurrency
  return $ Price (realToFrac amount) currency

parseCurrency :: Parser Currency
parseCurrency = do
  currency <- parseString
  return $ case currency of
    "USD" -> USD
    "EUR" -> EUR
    "JPY" -> JPY
    "GBP" -> GBP
    _ -> error "Unknown currency"

parseIngredients :: Parser Ingredients
parseIngredients = do
  a <- parseLiteral "ingredients:"
  ingredients <- parseIngredientList
  return $ Ingredients a ingredients

parseIngredientList :: Parser [Ingredient]
parseIngredientList = do
  firstIngredient <- parseIngredient
  rest <- optional (char ',' >> parseIngredientList)
  return $ case rest of
    Just otherIngredients -> firstIngredient : otherIngredients
    Nothing -> [firstIngredient]

parseIngredient :: Parser Ingredient
parseIngredient = do
  quantity <- parseQuantity
  name <- parseString
  return $ Ingredient quantity name

parseQuantity :: Parser Quantity
parseQuantity = do
  amount <- parseFloat
  unit <- parseUnit
  return $ Quantity (realToFrac amount) unit

parseUnit :: Parser Unit
parseUnit = do
  unit <- parseString
  return $ case unit of
    "ml" -> ML
    "oz" -> OZ
    "dash" -> Dash
    "splash" -> Splash
    _ -> error "Unknown unit"

-- Utility Parsers
sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = Parser $ \input ->
  let input' = dropWhile (== ' ') input
   in Right ((), input')

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseFloat :: Parser Float
parseFloat = Parser $ \input ->
  let (digits, rest) = span (\c -> c == '.' || isDigit c) (skipSpaces input)
   in if null digits
        then Left "Expected a float"
        else Right (read digits, rest)

parseString :: Parser String
parseString = Parser $ \input ->
  let input' = skipSpaces input
   in if null input'
        then Right ("", "")
        else
          if head input' == '"'
            then parseQuotedString (tail input')
            else
              let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
               in Right (str, rest)
  where
    parseQuotedString [] = Left "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = Right ("", rest)
    parseQuotedString (x : rest) = case parseQuotedString rest of
      Right (str, rest') -> Right (x : str, rest')
      Left err -> Left err

parseInt :: Parser Int
parseInt = Parser $ \input ->
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)