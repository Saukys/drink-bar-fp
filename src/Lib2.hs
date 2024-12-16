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

import Parsers

data State = State
  { money :: Double, -- in EUR
    inventory :: [Ingredient],
    menu :: [Drink]
  }
  deriving (Show, Eq)

parseQuery :: String -> Either String Query
parseQuery s =
  case parse parseTaskList s of
    (Left e, _) -> Left e
    (Right qs, r) -> if null r
      then case qs of
        [q] -> Right q
        _ -> Right (Sequence qs)
      else Left ("Unrecognized characters: " ++ r)

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
    Menu -> Right (Just ("Menu" ++ show (menu st)), st)
    ShowIngredients -> Right (Just $ show $ inventory st, st)
    Create drink -> Right (Just ("Created:" ++ show drink), st {menu = drink : menu st})
    Serve drink -> case findDrink drink (menu st) of
      Just d ->
        if canServe d st
          then Right (Just $ "Serving " ++ name d, removeIngredients d (addMoney (price d) st))
          else Left $ "no ingredients"
      Nothing -> Left "Drink not found"
    AddIngredient ingredient -> Right (Just $ "Added" ++ show ingredient, st {inventory = ingredient : inventory st})
    Money -> Right (Just $ show $ money st, st)
    Debug -> Right (Just $ show st, st)
    MoneyAdd p -> Right (Just $ "Added " ++ show p, addMoney p st)
    Sequence queryList ->
      foldl processQuery (Right (Just "", st)) queryList
      where
        processQuery :: Either String (Maybe String, State) -> Query -> Either String (Maybe String, State)
        processQuery (Left err) _ = Left err
        processQuery (Right (accMsg, currentState)) nextQuery =
          case stateTransition currentState nextQuery of
            Left err -> Left err
            Right (Just result, newState) ->
              Right (combineMessages accMsg (Just result), newState)
            Right (Nothing, newState) -> Right (accMsg, newState)

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

combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just msg) Nothing = Just msg
combineMessages Nothing (Just msg) = Just msg
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)