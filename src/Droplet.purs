module Droplet where

import Prelude

import Data.Array ((:))
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

{-
select fieldsList | * | function | scalars

from table name | sub select | select without table

where field op field | field op parameter | and/or

-}

--select

newtype Query = Query String

data Select (fieldsList :: Row Type) (name :: Symbol) = Select String (From fieldsList name)

class RowSelect (fieldsList :: RL.RowList Type) where
      toRowFieldList :: forall proxy. proxy fieldsList -> Array String

instance nilRowSelect :: RowSelect RL.Nil where
      toRowFieldList _ = []

instance consRowSelectable :: (RowSelect tail, IsSymbol field) => RowSelect (RL.Cons field v tail) where
      toRowFieldList _ = DS.reflectSymbol (Proxy :: Proxy field) : toRowFieldList (Proxy :: Proxy tail)

select :: forall fields fieldsList name. RL.RowToList fields fieldsList => RowSelect fieldsList => IsSymbol name => Proxy fields -> From fields name -> Query
select _ = toQuery <<< Select fieldsList
      where fieldsList = DST.joinWith ", " $ toRowFieldList (Proxy :: Proxy fieldsList)
            toQuery = case _ of
                  Select fields (FromTable table) -> Query $ "select " <> fields <> " from " <> show table

--from

data Table (fieldsList :: Row Type) (name :: Symbol) = Table

table :: forall fields fieldsList name. RL.RowToList fields fieldsList => RowSelect fieldsList => IsSymbol name => Table fields name
table = Table

instance tableShow :: IsSymbol name => Show (Table fields name) where
      show _ = DS.reflectSymbol (Proxy :: Proxy name)

instance tableQuery :: Show Query where
      show (Query query) = query

data From (fieldsList :: Row Type) (name :: Symbol) = FromTable (Table fieldsList name)

from :: forall fields fieldsList name. RL.RowToList fields fieldsList => IsSymbol name => RowSelect fieldsList => Table fields name -> From fields name
from table = FromTable table


--where

data Comparision a = Comparision String String (a -> a -> Boolean)

newtype Where (fields :: Row Type) = Where String

wher :: forall fields fieldsList. RL.RowToList fields fieldsList => RowSelect fieldsList => Array ?p -> Where fields
wher _ = ?i

--select ... from ... wher name .<>. name2 and name .=. name3 or name .>=. name4

