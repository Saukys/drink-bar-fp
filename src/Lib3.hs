{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command (..),
    Statements (..),
    atomicStatements

  )
where

import Control.Applicative (Alternative (many), (<|>))
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever, void)
import Data.List (intersperse)
import qualified Lib2
import Parsers
import System.Directory (doesFileExist)

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile "state.txt" s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist "state.txt"
      if exists
        then do
          s' <- readFile "state.txt"
          writeChan chan s'
        else writeChan chan ""

data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Show, Eq)

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = parse (StatementCommand <$> statements <|> parseLoad <|> parseSave)

parseLoad :: Parser Command
parseLoad = do
  _ <- parseLiteral "load"
  return LoadCommand

parseSave :: Parser Command
parseSave = do
  _ <- parseLiteral "save"
  return SaveCommand

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements = parse statements

statements :: Parser Statements
statements =
  ( do
      _ <- parseLiteral "BEGIN"
      _ <- parseLiteral "\n"
      q <-
        many
          ( do
              q <- parseTask
              _ <- parseLiteral ";"
              _ <- parseLiteral "\n"
              return q
          )
      _ <- parseLiteral "END"
      _ <- parseLiteral "\n"
      return $ Batch q
  )
    <|> (Single <$> parseTask)

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Batch queries
  where
    addIngredientQueries = map (\i -> Lib2.AddIngredient i) (Lib2.inventory state)
    createDrinkQueries = map (\d -> Lib2.Create d) (Lib2.menu state)
    addMoneyQuery = Lib2.MoneyAdd (Lib2.Price (Lib2.money state) Lib2.EUR)
    queries = addIngredientQueries ++ createDrinkQueries ++ [addMoneyQuery]

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file.
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements = \case
  Batch qs -> "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"
  Single q -> renderQuery q

renderQuery :: Lib2.Query -> String
renderQuery = \case
  Lib2.Create d -> "create( " ++ renderDrink d ++ ")"
  Lib2.Serve d -> "serve( " ++ renderDrink d ++ ")"
  Lib2.Menu -> "menu( " ++ ")"
  Lib2.ShowIngredients -> "show_ingredients( " ++ ")"
  Lib2.AddIngredient i -> "add( " ++ renderIngredient i ++ ")"
  Lib2.Money -> "money( " ++ ")"
  Lib2.Debug -> "debug( " ++ ")"
  Lib2.MoneyAdd p -> "add_money( " ++ renderPrice p ++ ")"

renderDrink :: Lib2.Drink -> String
renderDrink (Lib2.Drink n p i) = "\"" ++ n ++ "\" " ++ renderPrice p ++ " " ++ renderIngredients i

renderIngredients :: Lib2.Ingredients -> String
renderIngredients (Lib2.Ingredients _ i) = "ingredients: " ++ renderIngredientList i

renderIngredientList :: [Lib2.Ingredient] -> String
renderIngredientList i = concat $ intersperse ", " (map renderIngredient i)

renderIngredient :: Lib2.Ingredient -> String
renderIngredient (Lib2.Ingredient q n) = renderQuantity q ++ " \"" ++ n ++ "\""

renderQuantity :: Lib2.Quantity -> String
renderQuantity (Lib2.Quantity q u) = show q ++ " " ++ renderUnit u

renderUnit :: Lib2.Unit -> String
renderUnit = \case
  Lib2.ML -> "ml"
  Lib2.OZ -> "oz"
  Lib2.Dash -> "dash"
  Lib2.Splash -> "splash"

renderPrice :: Lib2.Price -> String
renderPrice (Lib2.Price p c) = show p ++ " " ++ renderCurrency c

renderCurrency :: Lib2.Currency -> String
renderCurrency = \case
  Lib2.USD -> "USD"
  Lib2.EUR -> "EUR"
  Lib2.JPY -> "JPY"
  Lib2.GBP -> "GBP"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  chan <- newChan :: IO (Chan ())
  writeChan ioChan (Save (renderStatements $ marshallState s') chan)
  readChan chan
  return $ Right $ Just "State saved successfully"
stateTransition s LoadCommand ioChan = do
  chan <- newChan :: IO (Chan String)
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if null qs
    then return (Left "No state file found")
    else case parseStatements qs of
      Left e -> do
        return $ Left $ "Failed to load state from file:\n" ++ e
      Right (qs', _) -> atomically $ atomicStatements s qs'
stateTransition s (StatementCommand sts) _ = atomically $ atomicStatements s sts

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList s (q : qs) = case Lib2.stateTransition s q of
  Left e -> Left e
  Right (msg, ns) ->
    if null qs
      then Right (msg, ns)
      else case transitionThroughList ns qs of
        Left e -> Left e
        Right (msg', ns') -> Right ((\x y -> x ++ "\n" ++ y) <$> msg <*> msg', ns')

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s (Batch qs) = do
  s' <- readTVar s
  case transitionThroughList s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      _ <- writeTVar s ns
      return $ Right msg
atomicStatements s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg