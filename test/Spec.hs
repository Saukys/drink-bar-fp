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
    [ testCase "parseQuery Create" $
      Lib2.parseQuery "Create Mojito 5.0EUR Ingredients: 50 ml Rum, 30 ml Lime Juice, 10 ml Sugar Syrup" @?= 
      Right (Lib2.Create (Lib2.Drink "Mojito" (Lib2.Price 5.0 Lib2.EUR) 
        [ Lib2.Ingredient (Lib2.Quantity 50 Lib2.ML) "Rum"
        , Lib2.Ingredient (Lib2.Quantity 30 Lib2.ML) "Lime Juice"
        , Lib2.Ingredient (Lib2.Quantity 10 Lib2.ML) "Sugar Syrup"
        ])),
      testCase "parseQuery Serve" $
      Lib2.parseQuery "Serve Mojito 5.0EUR Ingredients: 50 ml Rum, 30 ml Lime Juice, 10 ml Sugar Syrup" @?= 
      Right (Lib2.Serve (Lib2.Drink "Mojito" (Lib2.Price 5.0 Lib2.EUR) 
        [ Lib2.Ingredient (Lib2.Quantity 50 Lib2.ML) "Rum"
        , Lib2.Ingredient (Lib2.Quantity 30 Lib2.ML) "Lime Juice"
        , Lib2.Ingredient (Lib2.Quantity 10 Lib2.ML) "Sugar Syrup"
        ])),
      testCase "parseQuery Menu" $
      Lib2.parseQuery "Menu" @?= Right Lib2.Menu,
      testCase "parseQuery Ingredients" $
      Lib2.parseQuery "Ingredients" @?= Right Lib2.Ingredients,
      testCase "parseQuery AddIngredient" $
      Lib2.parseQuery "AddIngredient 50 ml Rum" @?= 
      Right (Lib2.AddIngredient (Lib2.Ingredient (Lib2.Quantity 50 Lib2.ML) "Rum")),
      testCase "parseQuery Invalid" $
      Lib2.parseQuery "Invalid Query" @?= Left "Invalid query"
    ]
    