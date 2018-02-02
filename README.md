# Simple-JSON Generic Sums

A demo/usable library for decoding sum types using Generics-Rep to decode from { tag, value } JSON.

More of a proof of concept. I do not recommend that anyone would use this over the Variant de/encoding in Simple-JSON. Really, if you read the source of this repo, you should probably realize what all is wrong with this approach.

## Tl;dr

```hs
class FieldsToRow fields (row :: # Type)

class RecordToFields fields (row :: # Type) where
  recordToFields :: { | row } -> fields

class ReadForeignGenericSum a where
  readForeignGenericSum :: Foreign -> F a

instance rfgsRec ::
  ( FieldsToRow fields row
  , RecordToFields fields row
  , ReadForeign (Record row)
  ) => ReadForeignGenericSum (Rec fields) where
...

instance rfgsCons ::
  ( IsSymbol name
  , ReadForeignGenericSum a
  ) => ReadForeignGenericSum (Constructor name a) where
...


data Fruit
  = Apple
  | Grapes Int
  | Thing { name :: String, count :: Int, color :: String }
derive instance gFruit :: Generic Fruit _
instance sFruit :: Show Fruit where
  show = genericShow

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
      a = runExcept $ genericReadForeignGenericSumJSON testJSON1

    pending $ show a
    -- (Right (Thing { color: "purple", count: 1, name: "watermelon" }))

    it "works" do
      isRight a `shouldEqual` true
```
