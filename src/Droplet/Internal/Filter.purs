module Droplet.Internal.Filter where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Droplet.Internal.Definition

data Operator =
      Equals |
      NotEquals

data Filtered =
      Operation String String Operator |
      And Filtered Filtered |
      Or Filtered Filtered

newtype Filters (fields :: Row Type) (parameters :: Row Type) = Filters Filtered

--for whatever reason, using the proxy parameters instead of the types generates an error in the Data.Symbol module
equals :: forall parameters fields field compared.
      ToCompared field fields parameters =>
      ToCompared compared fields parameters =>
      field -> compared -> Filters fields parameters
equals field compared = Filters $ Operation (toCompared field) (toCompared compared) Equals

notEquals :: forall parameters fields field compared.
      ToCompared field fields parameters =>
      ToCompared compared fields parameters =>
      field -> compared -> Filters fields parameters
notEquals field compared = Filters $ Operation (toCompared field) (toCompared compared) NotEquals

and :: forall fields parameters. Filters fields parameters -> Filters fields parameters -> Filters fields parameters
and (Filters first) (Filters second) = Filters (And first second)

or :: forall fields parameters. Filters fields parameters -> Filters fields parameters -> Filters fields parameters
or (Filters first) (Filters second) = Filters (Or first second)

infix 4 notEquals as .<>.
infix 4 equals as .=.
--left associativity is what sql uses
infixl 3 and as .&&.
infixl 2 or as .||.


--it d be nicer if field parsing was entirely in ToQuery....
class ToCompared :: forall k1 k2. Type -> k1 -> k2 -> Constraint
class ToCompared c fields parameters | c -> fields, c -> parameters  where
      toCompared :: c -> String

instance fieldToCompared :: (IsSymbol name, Cons name t e fields) => ToCompared (Field name) fields parameters where
      toCompared _ = DS.reflectSymbol (Proxy :: Proxy name)

instance parameterToCompared :: (IsSymbol name, Cons name t e parameters) => ToCompared (Parameter name) fields parameters where
      toCompared _ = "@" <> DS.reflectSymbol (Proxy :: Proxy name)