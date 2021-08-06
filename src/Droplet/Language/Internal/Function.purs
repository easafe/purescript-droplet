module Droplet.Language.Internal.Function (AllowOrderBy, Ob, class TextColumn, Nob, count, class ToCount, Aggregate(..), string_agg, class ToStringAgg) where

import Prelude

import Data.BigInt (BigInt)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Language.Internal.Definition (Default, Path, Star)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

-- fields parameter is needed to match later with ToProjection
data Aggregate inp s (fields :: Row Type) (ks :: AllowOrderBy) (out :: Type) =
      Count inp |
      StringAgg inp s


data AllowOrderBy

foreign import data Ob :: AllowOrderBy
foreign import data Nob :: AllowOrderBy


class ToCount (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToCount (Proxy name) fields

instance Cons name t e fields => ToCount (Path alias name) fields

instance ToCount Star fields


class ToStringAgg (f :: Type) (fields :: Row Type) | f -> fields

instance (Cons name t e fields, TextColumn t) => ToStringAgg (Proxy name) fields

instance (Cons name t e fields, TextColumn t) => ToStringAgg (Path alias name) fields


class TextColumn (t :: Type)

instance TextColumn String

instance TextColumn (Maybe String)

instance TextColumn (Default String)


count :: forall f fields. ToCount f fields => f -> Aggregate f Unit fields Nob BigInt
count = Count

--Maybe String because null
string_agg :: forall f fields. ToStringAgg f fields => f -> String -> Aggregate f String fields Ob (Maybe String)
string_agg f sep = StringAgg f sep