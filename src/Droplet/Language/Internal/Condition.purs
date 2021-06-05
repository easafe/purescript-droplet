-- | Logical operators for filtering records
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Condition (class ToCondition, class ToCompared, Op(..), and, Operator(..), equals, notEquals, greaterThan, lesserThan, or, (.&&.), (.<>.), (.=.), (.||.), (.<.), (.>.)) where

import Prelude

import Droplet.Language.Internal.Definition (class ToValue, class UnwrapDefinition, Path)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)


data Operator =
      Equals |
      NotEquals |
      GreaterThan |
      LesserThan |
      And |
      Or

derive instance opEq :: Eq Operator

data Op b c = Op Operator b c


--two type classes to not clutter Op with fields and alias type parameters
class ToCondition (c :: Type) (fields :: Row Type) (alias :: Symbol)

instance eToCondition :: (ToCondition (Op a b) fields alias, ToCondition (Op c d) fields alias) => ToCondition (Op (Op a b) (Op c d)) fields alias

else instance elseToCondition :: ToCompared a b fields alias => ToCondition (Op a b) fields alias


class ToCompared (c :: Type) (t :: Type) (fields :: Row Type) (alias :: Symbol) | c t -> fields

--boring
instance fieldFieldToCondition :: (
      Cons name t d fields,
      Cons otherName t e fields
) => ToCompared (Proxy name) (Proxy otherName) fields alias

else instance fieldParameterToCondition :: (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCompared (Proxy name) u fields alias

else instance parameterFieldToCondition :: (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCompared u (Proxy name) fields alias

else instance pToCondition :: (
      Cons name t d fields,
      Cons otherName t e fields
) => ToCompared (Path alias name) (Path alias otherName) fields alias

else instance p2ToCondition :: (
      Cons name t d fields,
      Cons otherName t e fields
) => ToCompared (Path alias name) (Proxy otherName) fields alias

else instance p3ToCondition :: (
      Cons name t d fields,
      Cons otherName t e fields
) => ToCompared (Proxy name) (Path alias otherName) fields alias

else instance p4ToCondition :: (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCompared (Path alias name) u fields alias

else instance p5ToCondition :: (
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCompared u (Path alias name) fields alias

else instance p7ToCondition :: Cons otherName t e fields => ToCompared (Path alias name) (Proxy otherName) fields alias

else instance p8ToCondition :: Cons name t d fields => ToCompared (Proxy name) (Path table otherName) fields alias

else instance p9ToCondition :: ToCompared (Path table name) u fields alias

else instance p10ToCondition :: ToCompared u (Path table name) fields alias

else instance parameterParameterToCondition :: ToValue s => ToCompared s s fields alias


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
--left associativity is what sql uses
infixl 3 and as .&&.
infixl 2 or as .||.

