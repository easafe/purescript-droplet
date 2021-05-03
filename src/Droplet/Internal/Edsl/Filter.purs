module Droplet.Internal.Edsl.Filter where

import Droplet.Internal.Edsl.Definition
import Prelude

import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

data IsParameterized

foreign import data Parameterized :: IsParameterized
foreign import data NotParameterized  :: IsParameterized

data Operator =
      Equals |
      NotEquals

data Filtered =
      Operation String String Operator |
      And Filtered Filtered |
      Or Filtered Filtered

newtype Filters (fields :: Row Type) (parameters :: Row Type) (has :: IsParameterized) = Filters Filtered

--it d be nicer if field parsing was entirely in ToQuery....
class ToCompared c (t :: Type) (fields :: Row Type) (parameters :: Row Type) | c -> fields, c -> t, c -> parameters where
      toCompared :: c -> String

instance fieldToCompared :: (IsSymbol name, Cons name t e fields) => ToCompared (Field name) t fields parameters where
      toCompared _ = DS.reflectSymbol (Proxy :: Proxy name)

instance parameterToCompared :: (IsSymbol name, Cons name t e parameters) => ToCompared (Parameter name) t fields parameters where
      toCompared _ = atToken <> DS.reflectSymbol (Proxy :: Proxy name)

-- oh well...
class HasParameter :: forall k1 k2. k1 -> k2 -> IsParameterized -> Constraint
class HasParameter c d (has :: IsParameterized) | c -> has, d -> has

instance dontHaveParameter :: (IsSymbol name, IsSymbol otherName) => HasParameter (Field name) (Field otherName) NotParameterized
else
instance stillDontHaveParameter :: HasParameter NotParameterized NotParameterized NotParameterized
else
instance hasParameter :: HasParameter fp pf Parameterized

equals :: forall parameters fields t field has compared.
      HasParameter field compared has =>
      ToCompared field t fields parameters =>
      ToCompared compared t fields parameters =>
      field -> compared -> Filters fields parameters has
equals field compared = Filters $ Operation (toCompared field) (toCompared compared) Equals

notEquals :: forall parameters has fields field t compared.
      HasParameter field compared has =>
      ToCompared field t fields parameters =>
      ToCompared compared t fields parameters =>
      field -> compared -> Filters fields parameters has
notEquals field compared = Filters $ Operation (toCompared field) (toCompared compared) NotEquals

and :: forall fields parameters has aHas otherHas. HasParameter aHas otherHas has => Filters fields parameters aHas -> Filters fields parameters otherHas -> Filters fields parameters has
and (Filters first) (Filters second) = Filters (And first second)

or :: forall fields aHas otherHas has parameters. HasParameter aHas otherHas has => Filters fields parameters aHas -> Filters fields parameters otherHas -> Filters fields parameters has
or (Filters first) (Filters second) = Filters (Or first second)

infix 4 notEquals as .<>.
infix 4 equals as .=.
--left associativity is what sql uses
infixl 3 and as .&&.
infixl 2 or as .||.

atToken :: String
atToken = "@"