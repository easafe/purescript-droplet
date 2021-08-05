module Droplet.Language.Internal.Function (AllowOrderBy, Ob, Nob, count, class ToCount, Aggregate(..), string_agg, class ToStringAgg) where

import Data.BigInt (BigInt)
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Definition (Path, Star)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

-- fields parameter is needed to match later with ToProjection
data Aggregate inp (fields :: Row Type) (ks :: AllowOrderBy) (out :: Type) =
      Count inp |
      StringAgg inp


data AllowOrderBy

foreign import data Ob :: AllowOrderBy
foreign import data Nob :: AllowOrderBy


class ToCount (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToCount (Proxy name) fields

instance Cons name t e fields => ToCount (Path alias name) fields

instance ToCount Star fields


class ToStringAgg (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToStringAgg (Proxy name /\ String) fields


count :: forall f fields. ToCount f fields => f -> Aggregate f fields Nob BigInt
count = Count

string_agg :: forall f fields. ToStringAgg f fields => f -> Aggregate f fields Ob String
string_agg = StringAgg