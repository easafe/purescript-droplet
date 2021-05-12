module Droplet.Internal.Edsl.Filter where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Droplet.Internal.Edsl.Definition (class ToValue, Field)
import Droplet.Internal.Edsl.Definition as DIED
import Foreign (Foreign)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

data Operator =
      Equals |
      NotEquals

data Filtered =
      Operation (Either Foreign String) (Either Foreign String) Operator |
      And Filtered Filtered |
      Or Filtered Filtered

newtype Filters (fields :: Row Type) = Filters Filtered

--it d be nicer if field parsing was entirely in ToQuery....
class ToFilter c (t :: Type) (fields :: Row Type) | c -> fields, c -> t where
      toCompared :: c -> Either Foreign String

instance fieldToCompared :: (IsSymbol name, Cons name t e fields) => ToFilter (Field name) t fields where
      toCompared _ = Right $ DS.reflectSymbol (Proxy :: Proxy name)

else instance parameterToCompared :: ToValue c => ToFilter c t fields where
      toCompared s = Left $ DIED.toValue s

equals :: forall fields t field compared. ToFilter field t fields => ToFilter compared t fields => field -> compared -> Filters fields
equals field compared = Filters $ Operation (toCompared field) (toCompared compared) Equals

notEquals :: forall compared fields field t. ToFilter field t fields => ToFilter compared t fields => field -> compared -> Filters fields
notEquals field compared = Filters $ Operation (toCompared field) (toCompared compared) NotEquals

and :: forall fields. Filters fields -> Filters fields -> Filters fields
and (Filters first) (Filters second) = Filters (And first second)

or :: forall fields. Filters fields -> Filters fields -> Filters fields
or (Filters first) (Filters second) = Filters (Or first second)

infix 4 notEquals as .<>.
infix 4 equals as .=.
--left associativity is what sql uses
infixl 3 and as .&&.
infixl 2 or as .||.

atToken :: String
atToken = "@"