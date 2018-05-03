module Test.Main where

import Prelude

import Data.Either (Either, isRight)
import Data.Foreign (ForeignError)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Record.Format (format)
import Effect (Effect)
import Effect.Console (log)
import Simple.JSON (class ReadForeign, readJSON)
import Simple.JSON.GenericSum (genericReadForeignGenericSum)
import Test.Assert (assert)
import Type.Prelude (SProxy(..))
import Type.Row (kind RowList)

data Fruit
  = Apple
  | Grapes Int
  | Bananas String String Int
  | Thing { name :: String, count :: Int, color :: String }
derive instance gFruit :: Generic Fruit _
instance sFruit :: Show Fruit where
  show Apple = "Apple"
  show (Grapes x) = "Grapes " <> show x
  show (Bananas a b c) = format (SProxy :: SProxy "Bananas {a} {b} {c}") { a, b, c: show c }
  show (Thing { name, count, color }) = format (SProxy :: SProxy "Thing name: {name}, count: {count}, color: {color}") { name, count: show count, color }
instance rfFruit :: ReadForeign Fruit where
  readImpl = genericReadForeignGenericSum

main :: Effect Unit
main = do
  -- "genericReadForeignGenericSumJSON"
  let
    testJSON1 = """
    {
      "type": "Thing",
      "value": { "name": "watermelon", "count": 1, "color": "purple" }
    }
    """

    a :: Either (NonEmptyList ForeignError) Fruit
    a = readJSON testJSON1

  log $ show a
  -- (Right (Thing { color: "purple", count: 1, name: "watermelon" }))

  -- "works"
  assert $ isRight a

  let
    testJSON2 = """
    {
      "type": "Bananas",
      "value": ["Green", "Big", 3]
    }
    """

    b :: Either (NonEmptyList ForeignError) Fruit
    b = readJSON testJSON2

  log $ show b
  -- (Right (Bananas "Green" "Big" 3))

  -- "works with product types"
  assert $ isRight b
