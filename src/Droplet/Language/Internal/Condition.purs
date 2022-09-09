-- | Logical operators for filtering records
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Condition (class ToCondition, class ValidComparision, OuterScope, In, class Comparison, Op(..), IsNotNull(..), isNotNull, IsNull(..), isNull, in_, and, Exists(..), Not(..), not, BinaryOperator(..), equals, notEquals, greaterThan, lesserThan, greaterEqualsThan, lesserEqualsThan, or, (.&&.), (.<>.), (.=.), (.||.), (.<.), (.>.), (.<=.), (.>=.)) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(..))
import Droplet.Language.Internal.Definition (class IsNullable, class ToValue, class UnwrapDefinition, class UnwrapNullable, Path)
import Prim.Row (class Cons)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy)

data BinaryOperator
      = Equals
      | NotEquals
      | GreaterThan
      | LesserThan
      | GreaterEqualsThan
      | LesserEqualsThan
      | And
      | Or

data In = In -- only for non empty arrays

data Exists = Exists

data Not = Not

data IsNotNull = IsNotNull

data IsNull = IsNull

derive instance Eq BinaryOperator

-- | Wrapper for comparisons
data Op b c = Op (Maybe BinaryOperator) b c

data OuterScope

-- | SQL logical expressions
class ToCondition (c ∷ Type) (fields ∷ Row Type) (alias ∷ Symbol)

-- | AND/OR
instance (ToCondition (Op a b) fields alias, ToCondition (Op c d) fields alias) ⇒ ToCondition (Op (Op a b) (Op c d)) fields alias

-- | EXISTS
else instance ToCondition (Op Exists b) fields alias

-- | IS NULL
else instance (Cons name t d fields, IsNullable t) ⇒ ToCondition (Op IsNull (Proxy name)) fields alias

else instance (Cons name t d fields, IsNullable t) ⇒ ToCondition (Op IsNull (Path alias name)) fields alias

else instance ToCondition (Op IsNull (Path table name)) fields alias

-- | IS NOT NULL
else instance (Cons name t d fields, IsNullable t) ⇒ ToCondition (Op IsNotNull (Proxy name)) fields alias

else instance (Cons name t d fields, IsNullable t) ⇒ ToCondition (Op IsNotNull (Path alias name)) fields alias

else instance ToCondition (Op IsNotNull (Path table name)) fields alias

-- | NOT
else instance ToCondition a fields alias ⇒ ToCondition (Op Not a) fields alias

-- | IN values
else instance ToCondition (Op a b) fields alias ⇒ ToCondition (Op In (Op a (NonEmptyArray b))) fields alias

-- | Comparisons
else instance
      ( Comparison a fields alias t
      , Comparison b fields alias u
      , ValidComparision t u
      ) ⇒
      ToCondition (Op a b) fields alias

-- | Whether expression can be compared
class Comparison (c ∷ Type) (fields ∷ Row Type) (alias ∷ Symbol) (t ∷ Type) | c → fields t

instance
      ( Cons name t d fields
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      Comparison (Proxy name) fields alias v

else instance
      ( Cons name t d fields
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      Comparison (Path alias name) fields alias v

else instance Comparison (Path table name) fields alias OuterScope

else instance Fail (Text "Comparisons must not be wrapped in Maybe") ⇒ Comparison (Maybe t) fields alias t

else instance ToValue t ⇒ Comparison t fields alias t

-- | Whether given types can be compared
class ValidComparision (t ∷ Type) (u ∷ Type)

instance ValidComparision t OuterScope

else instance ValidComparision OuterScope t

else instance ValidComparision t t

equals ∷ ∀ field other. field → other → Op field other
equals field other = Op (Just Equals) field other

notEquals ∷ ∀ compared field. field → compared → Op field compared
notEquals field compared = Op (Just NotEquals) field compared

greaterThan ∷ ∀ compared field. field → compared → Op field compared
greaterThan field compared = Op (Just GreaterThan) field compared

lesserThan ∷ ∀ compared field. field → compared → Op field compared
lesserThan field compared = Op (Just LesserThan) field compared

greaterEqualsThan ∷ ∀ compared field. field → compared → Op field compared
greaterEqualsThan field compared = Op (Just GreaterEqualsThan) field compared

lesserEqualsThan ∷ ∀ compared field. field → compared → Op field compared
lesserEqualsThan field compared = Op (Just LesserEqualsThan) field compared

and ∷ ∀ a b c d. Op a b → Op c d → Op (Op a b) (Op c d)
and first second = Op (Just And) first second

or ∷ ∀ a b c d. Op a b → Op c d → Op (Op a b) (Op c d)
or first second = Op (Just Or) first second

in_ ∷ ∀ compared field. field → compared → Op In (Op field compared)
in_ field compared = Op Nothing In (Op Nothing field compared)

not ∷ ∀ compared field. Op field compared → Op Not (Op field compared)
not a = Op Nothing Not a

isNull ∷ ∀ field. field → Op IsNull field
isNull field = Op Nothing IsNull field

isNotNull ∷ ∀ field. field → Op IsNotNull field
isNotNull field = Op Nothing IsNotNull field

infix 4 notEquals as .<>.
infix 4 equals as .=.
infix 4 greaterThan as .>.
infix 4 lesserThan as .<.
infix 4 greaterEqualsThan as .>=.
infix 4 lesserEqualsThan as .<=.
infixl 3 and as .&&.
infixl 2 or as .||.
