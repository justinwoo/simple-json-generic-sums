module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (Either, isRight)
import Data.Foreign (ForeignError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Simple.JSON (class ReadForeign, readJSON)
import Simple.JSON.GenericSum (genericReadForeignGenericSum, genericReadForeignGenericSumJSON)
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Type.Row (kind RowList)

data Fruit
  = Apple
  | Grapes Int
  | Thing { name :: String, count :: Int, color :: String }
derive instance gFruit :: Generic Fruit _
instance sFruit :: Show Fruit where
  show = genericShow
instance rfFruit :: ReadForeign Fruit where
  readImpl = genericReadForeignGenericSum

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "genericReadForeignGenericSumJSON" do
    let
      testJSON1 :: String
      testJSON1 = """
      {
        "type": "Thing",
        "value": { "name": "watermelon", "count": 1, "color": "purple" }
      }
      """

      a :: Either (NonEmptyList ForeignError) Fruit
      a = readJSON testJSON1

    pending $ show a
    -- (Right (Thing { color: "purple", count: 1, name: "watermelon" }))

    it "works" do
      isRight a `shouldEqual` true
