{-# LANGUAGE ImportQualifiedPost #-}

import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "parseQuery create" $
        Lib2.parseQuery "create drinkName 10.0 EUR ingredients: 10.0 ml ingredientName" @?=
        Right (Lib2.Create (Lib2.Drink "drinkName" (Lib2.Price 10.0 Lib2.EUR) (Lib2.Ingredients "ingredients: " [Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName"]))),
      testCase "parseQuery serve" $
        Lib2.parseQuery "serve drinkName 10.0 EUR ingredients: 10.0 ml ingredientName" @?=
        Right (Lib2.Serve (Lib2.Drink "drinkName" (Lib2.Price 10.0 Lib2.EUR) (Lib2.Ingredients "ingredients: " [Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName"]))),
      testCase "parseQuery menu" $
        Lib2.parseQuery "menu" @?= Right Lib2.Menu,
      testCase "parseQuery show ingredients" $
        Lib2.parseQuery "show ingredients" @?= Right Lib2.ShowIngredients,
      testCase "parseQuery add ingredient" $
        Lib2.parseQuery "add ingredient 10.0 ml ingredientName" @?=
        Right (Lib2.AddIngredient (Lib2.Ingredient (Lib2.Quantity 10.0 Lib2.ML) "ingredientName")),
      testCase "parseQuery money" $
        Lib2.parseQuery "money" @?= Right Lib2.Money
    ]
    