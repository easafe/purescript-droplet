-- | Logical operators for filtering records
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Internal.Language.Condition (class ToCondition, Condition(..), Filtered(..), OperationFields(..), Operator(..), and, equals, notEquals, greaterThan, lesserThan, or, toCondition, (.&&.), (.<>.), (.=.), (.||.), (.<.), (.>.)) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Droplet.Internal.Language.Definition (class ToValue, class UnwrapDefinition)
import Droplet.Internal.Language.Definition as DIED
import Foreign (Foreign)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

data Operator =
      Equals |
      NotEquals |
      GreaterThan |
      LesserThan

data Filtered =
      Operation OperationFields Operator |
      And Filtered Filtered |
      Or Filtered Filtered

data OperationFields = OperationFields (Either Foreign String) (Either Foreign String)

newtype Condition (fields :: Row Type) = Condition Filtered

--it d be nicer if field parsing was entirely in ToQuery....
class ToCondition c t (fields :: Row Type) | c -> fields, t -> fields where
      toCondition :: c -> t -> OperationFields

--boring
instance fieldFieldToCondition :: (
      IsSymbol name,
      IsSymbol otherName,
      Cons name t d fields,
      Cons otherName t e fields
) => ToCondition (Proxy name) (Proxy otherName) fields where
      toCondition name otherName = OperationFields (Right $ DS.reflectSymbol name) (Right $ DS.reflectSymbol otherName)

else instance fieldParameterToCondition :: (
      IsSymbol name,
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCondition (Proxy name) u fields where
      toCondition name p = OperationFields (Right $ DS.reflectSymbol name) (Left $ DIED.toValue p)

else instance parameterFieldToCondition :: (
      IsSymbol name,
      UnwrapDefinition t u,
      Cons name t d fields,
      ToValue u
) => ToCondition u (Proxy name) fields where
      toCondition p name = OperationFields (Left $ DIED.toValue p) (Right $ DS.reflectSymbol name)

else instance parameterParameterToCondition :: ToValue s => ToCondition s s fields where
      toCondition s t = OperationFields (Left $ DIED.toValue s) (Left $ DIED.toValue t)

equals :: forall fields field compared. ToCondition field compared fields => field -> compared -> Condition fields
equals = cond Equals

notEquals :: forall compared fields field. ToCondition field compared fields => field -> compared -> Condition fields
notEquals = cond NotEquals

greaterThan :: forall compared fields field. ToCondition field compared fields => field -> compared -> Condition fields
greaterThan = cond GreaterThan

lesserThan :: forall compared fields field. ToCondition field compared fields => field -> compared -> Condition fields
lesserThan = cond LesserThan

cond :: forall compared fields field. ToCondition field compared fields => Operator -> field -> compared -> Condition fields
cond op field compared = Condition $ Operation (toCondition field compared) op

and :: forall fields. Condition fields -> Condition fields -> Condition fields
and (Condition first) (Condition second) = Condition (And first second)

or :: forall fields. Condition fields -> Condition fields -> Condition fields
or (Condition first) (Condition second) = Condition (Or first second)

infix 4 notEquals as .<>.
infix 4 equals as .=.
infix 4 greaterThan as .>.
infix 4 lesserThan as .<.
--left associativity is what sql uses
infixl 3 and as .&&.
infixl 2 or as .||.

