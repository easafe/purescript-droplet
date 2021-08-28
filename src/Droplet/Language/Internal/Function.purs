module Droplet.Language.Internal.Function (class TextColumn, count, class ToCount, Aggregate(..), string_agg, class ToStringAgg, UserDefinedFunction(..), function, class MatchArgumentList, FunctionSignature) where

import Data.BigInt (BigInt)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Definition (class ToValue, class UnwrapDefinition, Default, E, Path, Star)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

-- fields parameter is needed to match later with ToProjection
data Aggregate input rest (fields :: Row Type) (output :: Type) =
      Count input |
      StringAgg input rest

data UserDefinedFunction (input :: Type) args (fields :: Row Type) (output :: Type) = UserDefinedFunction String args

type FunctionSignature input output = forall args fields. MatchArgumentList input args fields => args -> UserDefinedFunction input args fields output

class ToCount (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToCount (Proxy name) fields

instance Cons name t e fields => ToCount (Path alias name) fields

instance ToCount Star fields


class ToStringAgg (f :: Type) (rest :: Type) (fields :: Row Type) | f -> fields

instance (Cons name t e fields, TextColumn t) => ToStringAgg (Proxy name) String fields

instance (Cons name t e fields, TextColumn t) => ToStringAgg (Path alias name) String fields


-- | Extra class for clearer error messages
class TextColumn (t :: Type)

instance TextColumn String

instance TextColumn (Maybe String)

instance TextColumn (Default String)


class MatchArgumentList (input :: Type) (args :: Type) (fields :: Row Type)

instance (MatchArgumentList inp ar fields, MatchArgumentList ut gs fields) => MatchArgumentList (inp /\ ut) (ar /\ gs) fields

else instance (
      Cons name t d fields,
      UnwrapDefinition t u,
      ToValue u
) => MatchArgumentList u (Proxy name) fields

else instance ToValue t => MatchArgumentList t t fields


count :: forall f fields. ToCount f fields => f -> Aggregate f E fields BigInt
count = Count

--Maybe String because null
string_agg :: forall f rest fields. ToStringAgg f rest fields => f -> rest -> Aggregate f rest fields (Maybe String)
string_agg f rest = StringAgg f rest

-- | Represents a user defined function
function :: forall input output args fields. MatchArgumentList input args fields => String -> args -> UserDefinedFunction input args fields output
function name args = UserDefinedFunction name args