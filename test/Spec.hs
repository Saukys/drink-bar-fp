{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib2 qualified
import Lib3 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Control.Concurrent.STM (newTVarIO, atomically, readTVarIO, modifyTVar)
import Control.Concurrent (forkIO, newChan)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "parseQuery create" $
        Lib2.parseQuery "create( drinkName 10.0 EUR ingredients: 10.0 ml ingredientName)"
          @?= Right (Lib2.Create (Lib2.Drink "drinkName" (Lib2.Price 10.0 Lib2.EUR) (Lib2.Ingredients "" [Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName"]))),
      testCase "parseQuery serve" $
        Lib2.parseQuery "serve( drinkName 10.0 EUR ingredients: 10.0 ml ingredientName)"
          @?= Right (Lib2.Serve (Lib2.Drink "drinkName" (Lib2.Price 10.0 Lib2.EUR) (Lib2.Ingredients "" [Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName"]))),
      testCase "parseQuery menu" $
        Lib2.parseQuery "menu()" @?= Right Lib2.Menu,
      testCase "parseQuery show_ingredients()" $
        Lib2.parseQuery "ingredients()" @?= Right Lib2.ShowIngredients,
      testCase "parseQuery add ingredient" $
        Lib2.parseQuery "add(10.0 ml ingredientName)"
          @?= Right (Lib2.AddIngredient (Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName")),
      testCase "parseQuery money" $
        Lib2.parseQuery "profits()" @?= Right Lib2.Money,
      testCase "parseQuery debug" $
        Lib2.parseQuery "debug()" @?= Right Lib2.Debug,
      testCase "parseQuery money add" $
        Lib2.parseQuery "earn_money(10.0 EUR)" @?= Right (Lib2.MoneyAdd (Lib2.Price 10.0 Lib2.EUR)),
      testCase "SaveCommand state transition" $ do
        initialState <- newTVarIO Lib2.emptyState
        ioChan <- newChan
        _ <- forkIO $ Lib3.storageOpLoop ioChan
        result <- Lib3.stateTransition initialState Lib3.SaveCommand ioChan
        result @?= Right (Just "State saved successfully"),
      testCase "StatementCommand state transition" $ do
        initialState <- newTVarIO Lib2.emptyState
        ioChan <- newChan
        _ <- forkIO $ Lib3.storageOpLoop ioChan
        let statements = Lib3.Single (Lib2.AddIngredient (Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName"))
        result <- Lib3.stateTransition initialState (Lib3.StatementCommand statements) ioChan
        updatedState <- readTVarIO initialState
        result @?= Right (Just "AddedIngredient (Quantity 10.0 ML) \"ingredientName\"")
        Lib2.inventory updatedState @?= [Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName"],
      testCase "renderStatements and parseStatements" $ do
        let statements = Lib3.Batch [Lib2.AddIngredient (Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName"), Lib2.MoneyAdd (Lib2.Price 10.0 Lib2.EUR)]
        let rendered = Lib3.renderStatements statements
        let parsed = Lib3.parseStatements rendered
        parsed @?= Right (statements, "")
    ]
    


propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]