{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
    Parser,
    parse
  )
where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
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

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

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
  _ <- parseLiteral "earn_money"
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
  _ <- parseLiteral "ingredients"
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
  _ <- parseLiteral "profits"
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
sat p = do
  input <- lift get
  case input of
    [] -> throwError "Empty String"
    (x:xs) -> if p x
              then lift (put xs) >> return x
              else throwError $ "Could not recognize: " ++ [x]
              
char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = lift (modify (dropWhile (== ' ')))

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseFloat :: Parser Float
parseFloat = do
  input <- lift get
  let (digits, rest) = span (\c -> c == '.' || isDigit c) (skipSpaces input)
  if null digits
    then throwError "Expected a float"
    else do
      lift (put rest)
      return (read digits)


parseString :: Parser String
parseString = do
  input <- lift get
  let input' = skipSpaces input
  if null input'
    then return ""
    else if head input' == '"'
         then parseQuotedString (tail input')
         else let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
              in lift (put rest) >> return str
  where
    parseQuotedString [] = throwError "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = lift (put rest) >> return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)

parseInt :: Parser Int
parseInt = do
  input <- lift get
  let (digits, rest) = span isDigit (skipSpaces input)
  if null digits
    then throwError "Expected an integer"
    else do
      lift (put rest)
      return (read digits)