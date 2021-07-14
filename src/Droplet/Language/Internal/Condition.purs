-- | Logical operators for filtering records
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Condition (class ToCondition, class ToComparison, Op(..), and, Operator(..), equals, notEquals, greaterThan, lesserThan, or, (.&&.), (.<>.), (.=.), (.||.), (.<.), (.>.)) where

import Prelude

import Droplet.Language.Internal.Definition (class ToValue, class UnwrapDefinition, Path)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

-- | SQL logic/comparision operators
data Operator =
      Equals |
      NotEquals |
      GreaterThan |
      LesserThan |
      And |
      Exists |
      Or

derive instance Eq Operator

-- | Wrapper for comparisions
data Op b c = Op Operator b c


-- | SQL logical expressions
class ToCondition (c :: Type) (fields :: Row Type) (alias :: Symbol)

-- | AND/OR
instance (ToCondition (Op a b) fields alias, ToCondition (Op c d) fields alias) => ToCondition (Op (Op a b) (Op c d)) fields alias

-- | Comparisions
else instance ToComparison a b fields alias => ToCondition (Op a b) fields alias


-- | Whether expressions can be compared
class ToComparison (c :: Type) (t :: Type) (fields :: Row Type) (alias :: Symbol) | c t -> fields

instance (
      Cons name t d fields,
      Cons otherName t e fields
) => ToComparison (Proxy name) (Proxy otherName) fields alias

else instance (
      Cons name t d fields,
      Cons otherName t e fields
) => ToComparison (Path alias name) (Path alias otherName) fields alias

else instance (
      Cons name t d fields,
      Cons otherName t e fields
) => ToComparison (Path alias name) (Proxy otherName) fields alias

else instance (
      Cons name t d fields,
      Cons otherName t e fields
) => ToComparison (Proxy name) (Path alias otherName) fields alias

else instance Cons otherName t e fields => ToComparison (Path table name) (Proxy otherName) fields alias

else instance Cons name t d fields => ToComparison (Proxy name) (Path table otherName) fields alias

else instance ToComparison (Path table name) (Path alias otherName) fields alias

else instance ToComparison (Path alias otherName) (Path table name)  fields alias

else instance (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToComparison (Path alias name) u fields alias

else instance (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToComparison (Proxy name) u fields alias

else instance (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToComparison u (Proxy name) fields alias

else instance (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToComparison u (Path alias name) fields alias

else instance ToValue u => ToComparison (Path table name) u fields alias

-- | Exists
else instance ToComparison Unit q fields alias

else instance ToValue u => ToComparison u (Path table name) fields alias

else instance ToValue s => ToComparison s s fields alias


equals :: forall field compared. field -> compared -> Op field compared
equals field compared = Op Equals field compared

notEquals :: forall compared field. field -> compared -> Op field compared
notEquals field compared = Op NotEquals field compared

greaterThan :: forall compared field. field -> compared -> Op field compared
greaterThan field compared = Op GreaterThan field compared

lesserThan :: forall compared field. field -> compared -> Op field compared
lesserThan field compared = Op LesserThan field compared

and :: forall a b c d. Op a b -> Op c d -> Op (Op a b) (Op c d)
and first second = Op And first second

or :: forall a b c d. Op a b -> Op c d -> Op (Op a b) (Op c d)
or first second = Op Or first second

infix 4 notEquals as .<>.
infix 4 equals as .=.
infix 4 greaterThan as .>.
infix 4 lesserThan as .<.
infixl 3 and as .&&.
infixl 2 or as .||.

