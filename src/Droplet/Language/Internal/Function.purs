module Droplet.Language.Internal.Function (class TextColumn, count, class ToCount, Aggregate(..), string_agg, class ToStringAgg) where


import Data.BigInt (BigInt)
import Data.Maybe (Maybe)
import Droplet.Language.Internal.Definition (Default, E, Path, Star)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

-- fields parameter is needed to match later with ToProjection
data Aggregate inp rest (fields :: Row Type) (out :: Type) =
      Count inp |
      StringAgg inp rest


class ToCount (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToCount (Proxy name) fields

instance Cons name t e fields => ToCount (Path alias name) fields

instance ToCount Star fields


class ToStringAgg (f :: Type) (rest :: Type) (fields :: Row Type) | f -> fields

instance (Cons name t e fields, TextColumn t) => ToStringAgg (Proxy name) String fields

instance (Cons name t e fields, TextColumn t) => ToStringAgg (Path alias name) String fields


class TextColumn (t :: Type)

instance TextColumn String

instance TextColumn (Maybe String)

instance TextColumn (Default String)


count :: forall f fields. ToCount f fields => f -> Aggregate f E fields BigInt
count = Count

--Maybe String because null
string_agg :: forall f rest fields. ToStringAgg f rest fields => f -> rest -> Aggregate f rest fields (Maybe String)
string_agg f rest = StringAgg f rest