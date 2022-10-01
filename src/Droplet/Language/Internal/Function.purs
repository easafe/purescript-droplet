module Droplet.Language.Internal.Function
      ( class TextColumn
      , count
      , class ToCount
      , random
      , Aggregate(..)
      , function'
      , string_agg
      , class ToStringAgg
      , class ToArrayAgg
      , class ToCoalesce
      , PgFunction(..)
      , array_agg
      , class MatchArgument
      , function
      , class MatchArgumentList
      , FunctionSignature
      , FunctionSignature'
      , coalesce
      ) where

import Prelude

import Data.BigInt (BigInt)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Definition (class AppendPath, class ToValue, class UnwrapDefinition, class UnwrapNullable, E, Path, Star)
import Prim.Row (class Cons)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy)

-- fields parameter is needed to match later with ToProjection
-- | Built-in aggregate functions
data Aggregate input rest (fields ∷ Row Type) (output ∷ Type)
      = Count input
      | StringAgg input rest
      | ArrayAgg input

-- | Declares a functions
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

class ToArrayAgg (f ∷ Type) (fields ∷ Row Type) (t ∷ Type) | f → fields t

instance (Cons name t e fields, UnwrapDefinition t u) ⇒ ToArrayAgg (Proxy name) fields u

instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e fields
      , UnwrapDefinition t u
      ) ⇒
      ToArrayAgg (Path alias name) fields u

-- | Function arguments must match input type
class MatchArgumentList (input ∷ Type) (args ∷ Type) (fields ∷ Row Type)

instance (MatchArgumentList inp ar fields, MatchArgumentList ut gs fields) ⇒ MatchArgumentList (inp /\ ut) (ar /\ gs) fields

else instance (MatchArgument i fields t, MatchArgument a fields t) ⇒ MatchArgumentList i a fields

-- | coalesce arguments must be of same type
class ToCoalesce (a ∷ Type) (fields ∷ Row Type) (t ∷ Type) | a → t

instance (ToCoalesce inp fields t, ToCoalesce ut fields t) ⇒ ToCoalesce (inp /\ ut) fields t

else instance MatchArgument i fields t ⇒ ToCoalesce i fields t

class MatchArgument (a ∷ Type) (fields ∷ Row Type) (t ∷ Type) | a → t

instance
      ( Cons name t d fields
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      MatchArgument (Proxy name) fields v

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t d fields
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      MatchArgument (Path alias name) fields v

else instance (UnwrapNullable o t, TypeEquals fd fields) ⇒ MatchArgument (PgFunction i a fd o) fields t

else instance (ToValue a, UnwrapNullable a t) ⇒ MatchArgument a fields t

count ∷ ∀ f fields. ToCount f fields ⇒ f → Aggregate f E fields BigInt
count = Count

--Maybe String because null
string_agg ∷ ∀ f rest fields. ToStringAgg f rest fields ⇒ f → rest → Aggregate f rest fields (Maybe String)
string_agg f rest = StringAgg f rest

--Maybe t because null
array_agg ∷ ∀ f t fields. ToArrayAgg f fields t ⇒ f → Aggregate f E fields (Maybe (Array t))
array_agg f = ArrayAgg f

random ∷ FunctionSignature' Number
random = function' "random"

coalesce ∷ ∀ input output fields. ToCoalesce input fields output ⇒ input → PgFunction input input fields (Maybe output)
coalesce = PgFunction "coalesce"

-- | Represents a function that takes arguments
function ∷ ∀ input output. String → FunctionSignature input output
function name args = PgFunction name args

-- | Represents a function that takes no arguments
function' ∷ ∀ output. String → FunctionSignature' output
function' name = PgFunction name unit