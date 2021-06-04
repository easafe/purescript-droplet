-- | Logical operators for filtering records
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Op (class ToCompared, Op(..), and, OpType, And, Equals, NotEquals, Or, LesserThan, GreaterThan, equals, notEquals, greaterThan, lesserThan, or, (.&&.), (.<>.), (.=.), (.||.), (.<.), (.>.)) where

import Data.Symbol (class IsSymbol)
import Droplet.Language.Internal.Definition (class ToValue, class UnwrapDefinition)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)


data OpType

foreign import data Equals :: OpType
foreign import data NotEquals  :: OpType
foreign import data GreaterThan  :: OpType
foreign import data LesserThan :: OpType

foreign import data And :: OpType
foreign import data Or :: OpType

data Op (fields :: Row Type) (a :: OpType) b c = Op b c


class ToCompared (c :: Type) (t :: Type) (fields :: Row Type) | c t -> fields

--boring
instance fieldFieldToCondition :: (
      IsSymbol name,
      IsSymbol otherName,
      Cons name t d fields,
      Cons otherName t e fields
) => ToCompared (Proxy name) (Proxy otherName) fields

else instance fieldParameterToCondition :: (
      IsSymbol name,
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCompared (Proxy name) u fields

else instance parameterFieldToCondition :: (
      IsSymbol name,
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCompared u (Proxy name) fields

else instance parameterParameterToCondition :: ToValue s => ToCompared s s fields


equals :: forall fields field compared. ToCompared field compared fields => field -> compared -> Op fields Equals field compared
equals field compared = Op field compared

notEquals :: forall compared fields field. ToCompared field compared fields => field -> compared -> Op fields NotEquals field compared
notEquals field compared = Op field compared

greaterThan :: forall compared fields field. ToCompared field compared fields => field -> compared -> Op fields GreaterThan field compared
greaterThan field compared = Op field compared

lesserThan :: forall compared fields field. ToCompared field compared fields => field -> compared -> Op fields LesserThan field compared
lesserThan field compared = Op field compared

and :: forall fields a b c d e f. Op fields a b c -> Op fields d e f -> Op fields And (Op fields a b c) (Op fields d e f)
and first second = Op first second

or :: forall fields a b c d e f. Op fields a b c -> Op fields d e f -> Op fields Or (Op fields a b c) (Op fields d e f)
or first second = Op first second

infix 4 notEquals as .<>.
infix 4 equals as .=.
infix 4 greaterThan as .>.
infix 4 lesserThan as .<.
--left associativity is what sql uses
infixl 3 and as .&&.
infixl 2 or as .||.

