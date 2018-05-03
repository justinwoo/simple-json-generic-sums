module Simple.JSON.GenericSum where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Foreign (F, Foreign, ForeignError(ForeignError), readString)
import Data.Foreign.Index (readIndex, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), to)
import Simple.JSON (class ReadForeign, read')
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (kind RowList)

class ReadForeignGenericProduct a where
  readForeignGenericProduct :: Int -> Foreign -> F a

instance rfgpProduct ::
  ( ReadForeignGenericProduct a
  , ReadForeignGenericProduct b
  ) => ReadForeignGenericProduct (Product a b) where
  readForeignGenericProduct i f
        = Product
      <$> readForeignGenericProduct i f
      <*> readForeignGenericProduct (i + 1) f

instance rfgpArg ::
  ( ReadForeign a
  ) => ReadForeignGenericProduct (Argument a) where
  readForeignGenericProduct i f = do
    Argument <$> (read' =<< readIndex i f)

class ReadForeignGenericSum a where
  readForeignGenericSum :: Foreign -> F a

instance rfgsSum ::
  ( ReadForeignGenericSum a
  , ReadForeignGenericSum b
  ) => ReadForeignGenericSum (Sum a b) where
  readForeignGenericSum f
      = Inl <$> readForeignGenericSum f
    <|> Inr <$> readForeignGenericSum f

instance rfgsNoArg :: ReadForeignGenericSum NoArguments where
  readForeignGenericSum _ =
    pure NoArguments

instance rfgsArg ::
  ( ReadForeign a
  ) => ReadForeignGenericSum (Argument a) where
  readForeignGenericSum f =
    Argument <$> (read' =<< readProp "value" f)

instance rfgsProduct ::
  ( ReadForeignGenericProduct (Product a b)
  ) => ReadForeignGenericSum (Product a b) where
  readForeignGenericSum f =
    readForeignGenericProduct 0 =<< readProp "value" f

instance rfgsCons ::
  ( IsSymbol name
  , ReadForeignGenericSum a
  ) => ReadForeignGenericSum (Constructor name a) where
  readForeignGenericSum f = do
    ty <- readString =<< readProp "type" f
    if name == ty
      then
        Constructor <$> readForeignGenericSum f
      else
        throwError <<< pure <<< ForeignError
          $ "could not match given " <> ty <> " with " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)

genericReadForeignGenericSum :: forall a rep
   . Generic a rep
  => ReadForeignGenericSum rep
  => Foreign
  -> F a
genericReadForeignGenericSum f = to <$> readForeignGenericSum f

genericReadForeignGenericSumJSON :: forall a rep
   . Generic a rep
  => ReadForeignGenericSum rep
  => String
  -> F a
genericReadForeignGenericSumJSON s = genericReadForeignGenericSum =<< parseJSON s
