{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Control.Lens hiding (view)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
import Data.String.Conversions (cs)
import Network.Wreq hiding (get)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (intersperse)
import System.Environment (getArgs)

data Command next = AddIngredient String String next
    | CreateDrink String Double [(String, String)] next
    | ServeDrink String Double [(String, String)] next
    | AddMoney Double next
    | ShowMenu (String -> next)
    | ShowIngredients (String -> next)
    | ShowMoney (String -> next)
    deriving (Functor)

type ProgramDSL = Free Command

addIngredient :: String -> String -> ProgramDSL ()
addIngredient quantity ingredient = liftF $ AddIngredient quantity ingredient ()

createDrink :: String -> Double -> [(String, String)] -> ProgramDSL ()
createDrink name price ingredients = liftF $ CreateDrink name price ingredients ()

serveDrink :: String -> Double -> [(String, String)] -> ProgramDSL ()
serveDrink name price ingredients = liftF $ ServeDrink name price ingredients ()

addMoney :: Double -> ProgramDSL ()
addMoney amount = liftF $ AddMoney amount ()

showMenu :: ProgramDSL String
showMenu = liftF $ ShowMenu id

showIngredients :: ProgramDSL String
showIngredients = liftF $ ShowIngredients id

showMoney :: ProgramDSL String
showMoney = liftF $ ShowMoney id

formatIngredients :: [(String, String)] -> String
formatIngredients = concat . intersperse ", " . map (\(quantity, ingredient) -> quantity ++ " \"" ++ ingredient ++ "\"")

runHttpSingle :: ProgramDSL a -> IO a
runHttpSingle (Pure a) = return a
runHttpSingle (Free (AddIngredient quantity ingredient next)) = do
    resp <- post "http://localhost:3000" (cs $ "add(" ++ quantity ++ " \"" ++ ingredient ++ "\")" :: ByteString)
    runHttpSingle next
runHttpSingle (Free (CreateDrink name price ingredients next)) = do
    let formattedIngredients = formatIngredients ingredients
    resp <- post "http://localhost:3000" (cs $ "create(" ++ name ++ " " ++ show price ++ " EUR ingredients: " ++ formattedIngredients ++ ")" :: ByteString)
    runHttpSingle next
runHttpSingle (Free (ServeDrink name price ingredients next)) = do
    let formattedIngredients = formatIngredients ingredients
    resp <- post "http://localhost:3000" (cs $ "serve(" ++ name ++ " " ++ show price ++ " EUR ingredients: " ++ formattedIngredients ++ ")" :: ByteString)
    runHttpSingle next
runHttpSingle (Free (AddMoney amount next)) = do
    resp <- post "http://localhost:3000" (cs $ "earn_money(" ++ show amount ++ " EUR)" :: ByteString)
    runHttpSingle next
runHttpSingle (Free (ShowMenu f)) = do
    resp <- post "http://localhost:3000" (cs "menu()" :: ByteString)
    runHttpSingle $ f $ cs $ resp ^. responseBody
runHttpSingle (Free (ShowIngredients f)) = do
    resp <- post "http://localhost:3000" (cs "ingredients()" :: ByteString)
    runHttpSingle $ f $ cs $ resp ^. responseBody
runHttpSingle (Free (ShowMoney f)) = do
    resp <- post "http://localhost:3000" (cs "profit()" :: ByteString)
    runHttpSingle $ f $ cs $ resp ^. responseBody

runHttpBatch :: ProgramDSL a -> IO a
runHttpBatch = runHttpBatch' []

runHttpBatch' :: [String] -> ProgramDSL a -> IO a
runHttpBatch' acc (Pure a) = do
    unless (null acc) $ do
        putStrLn $ "Batched requests: " ++ unlines acc
        _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
        return ()
    return a
runHttpBatch' acc (Free (AddIngredient quantity ingredient next)) = runHttpBatch' (acc ++ ["add(" ++ quantity ++ " \"" ++ ingredient ++ "\")"]) next
runHttpBatch' acc (Free (CreateDrink name price ingredients next)) = runHttpBatch' (acc ++ ["create(" ++ name ++ " " ++ show price ++ " EUR ingredients: " ++ formatIngredients ingredients ++ ")"]) next
runHttpBatch' acc (Free (ServeDrink name price ingredients next)) = runHttpBatch' (acc ++ ["serve(" ++ name ++ " " ++ show price ++ " EUR ingredients: " ++ formatIngredients ingredients ++ ")"]) next
runHttpBatch' acc (Free (AddMoney amount next)) = runHttpBatch' (acc ++ ["earn_money(" ++ show amount ++ " EUR)"]) next
runHttpBatch' acc (Free (ShowMenu f)) = do
    unless (null acc) $ do
        putStrLn $ "Batched requests: " ++ unlines acc
        _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
        return ()
    resp <- post "http://localhost:3000" (cs "menu()" :: ByteString)
    runHttpBatch' [] (f $ cs $ resp ^. responseBody)
runHttpBatch' acc (Free (ShowIngredients f)) = do
    unless (null acc) $ do
        putStrLn $ "Batched requests: " ++ unlines acc
        _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
        return ()
    resp <- post "http://localhost:3000" (cs "ingredients()" :: ByteString)
    runHttpBatch' [] (f $ cs $ resp ^. responseBody)
runHttpBatch' acc (Free (ShowMoney f)) = do
    unless (null acc) $ do
        putStrLn $ "Batched requests: " ++ unlines acc
        _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
        return ()
    resp <- post "http://localhost:3000" (cs "profit()" :: ByteString)
    runHttpBatch' [] (f $ cs $ resp ^. responseBody)

type InMemoryState = ([(String, Double, [(String, String)])], Double, [(String, String)])

runInMemory :: ProgramDSL a -> StateT InMemoryState IO a
runInMemory (Pure a) = return a
runInMemory (Free (AddIngredient quantity ingredient next)) = do
    (drinks, money, ingredients) <- get
    Control.Monad.State.put (drinks, money, ingredients ++ [(quantity, ingredient)])
    runInMemory next
runInMemory (Free (CreateDrink name price drinkIngredients next)) = do
    (drinks, money, ingredients) <- get
    Control.Monad.State.put ((name, price, drinkIngredients) : drinks, money, ingredients)
    runInMemory next
runInMemory (Free (ServeDrink name price ingredients next)) = do
    (drinks, money, ingredients) <- get
    Control.Monad.State.put (drinks, money + price, ingredients)
    runInMemory next
runInMemory (Free (AddMoney amount next)) = do
    (drinks, money, ingredients) <- get
    Control.Monad.State.put (drinks, money + amount, ingredients)
    runInMemory next
runInMemory (Free (ShowMenu f)) = do
    (drinks, money, ingredients) <- get
    runInMemory $ f $ unlines $ map (\(name, price, drinkIngredients) -> name ++ " " ++ show price ++ " ingredients: " ++ show drinkIngredients) drinks
runInMemory (Free (ShowIngredients f)) = do
    (drinks, money, ingredients) <- get
    runInMemory $ f $ unlines $ map (\(quantity, ingredient) -> quantity ++ " " ++ ingredient) ingredients
runInMemory (Free (ShowMoney f)) = do
    (drinks, money, ingredients) <- get
    runInMemory $ f $ show money

main :: IO ()
main = do
    args <- getArgs
    let program = do
            addIngredient "1000 ml" "vodka"
            addIngredient "1000 ml" "orange juice"
            createDrink "screwdriver" 5 [("10 ml", "vodka"), ("10 ml", "orange juice")]
            _ <- showMenu
            serveDrink "screwdriver" 5 [("10 ml", "vodka"), ("10 ml", "orange juice")]
            addMoney 5
            _ <- showMoney
            showIngredients

    case args of
        ["http"] -> do
            putStrLn "Running in HTTP mode"
            _ <- runHttpSingle program
            return ()
        ["http-batch"] -> do
            putStrLn "Running in HTTP batch mode"
            _ <- runHttpBatch program
            return ()
        ["in-memory"] -> do
            putStrLn "Running in in-memory mode"
            (result, state) <- runStateT (runInMemory program) ([], 0, [])
            print result
            print state
            return ()
        _ -> putStrLn "Usage: stack exec fp2024-four-client (http|http-batch|in-memory)"
