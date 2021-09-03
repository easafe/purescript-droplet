module Droplet.Language.Internal.Function (class TextColumn, count, class ToCount, random, Aggregate(..), function', string_agg, class ToStringAgg, PgFunction(..), function, class MatchArgumentList, FunctionSignature, FunctionSignature') where

import Prelude

import Data.BigInt (BigInt)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Definition (class AppendPath, class ToValue, class UnwrapDefinition, class UnwrapNullable, Default, E, Path, Star)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

-- fields parameter is needed to match later with ToProjection
data Aggregate input rest (fields ∷ Row Type) (output ∷ Type)
      = Count input
      | StringAgg input rest

data PgFunction (input ∷ Type) args (fields ∷ Row Type) (output ∷ Type) = PgFunction String args

type FunctionSignature input output = ∀ args fields. MatchArgumentList input args fields ⇒ args → PgFunction input args fields output

type FunctionSignature' output = ∀ fields. PgFunction Void Unit fields output

class ToCount (f ∷ Type) (fields ∷ Row Type) | f → fields

instance Cons name t e fields ⇒ ToCount (Proxy name) fields

instance Cons name t e fields ⇒ ToCount (Path alias name) fields

instance ToCount Star fields

class ToStringAgg (f ∷ Type) (rest ∷ Type) (fields ∷ Row Type) | f → fields

instance (Cons name t e fields, TextColumn t) ⇒ ToStringAgg (Proxy name) String fields

instance (Cons name t e fields, TextColumn t) ⇒ ToStringAgg (Path alias name) String fields

-- | Extra class for clearer error messages
class TextColumn (t ∷ Type)

instance TextColumn String

instance TextColumn (Maybe String)

instance TextColumn (Default String)

--should support out of scope paths too
class MatchArgumentList (input ∷ Type) (args ∷ Type) (fields ∷ Row Type)

instance (MatchArgumentList inp ar fields, MatchArgumentList ut gs fields) ⇒ MatchArgumentList (inp /\ ut) (ar /\ gs) fields

else instance
      ( Cons name t d fields
      , UnwrapDefinition t u
      , UnwrapNullable u v
      , ToValue v
      ) ⇒
      MatchArgumentList v (Proxy name) fields

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t d fields
      , UnwrapDefinition t u
      , UnwrapNullable u v
      , ToValue v
      ) ⇒
      MatchArgumentList v (Path alias name) fields

else instance ToValue t ⇒ MatchArgumentList t t fields

count ∷ ∀ f fields. ToCount f fields ⇒ f → Aggregate f E fields BigInt
count = Count

--Maybe String because null
string_agg ∷ ∀ f rest fields. ToStringAgg f rest fields ⇒ f → rest → Aggregate f rest fields (Maybe String)
string_agg f rest = StringAgg f rest

random ∷ FunctionSignature' Number
random = function' "random"

-- | Represents a function that takes arguments
function ∷ ∀ input output. String → FunctionSignature input output
function name args = PgFunction name args

-- | Represents a function that takes no arguments
function' ∷ ∀ output. String → FunctionSignature' output
function' name = PgFunction name unit