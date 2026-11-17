module Conflict.ExcludedColumnType.Test where

import Droplet.Language
import Prelude hiding (join)
import Prim hiding (Constraint)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Language.Internal.Translate as DLIQ
import Type.Proxy (Proxy(..))

type Users =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      , surname ∷ String
      )

users ∷ Table "users" Users
users = Table

type UniqueValues =
      ( name ∷ Column String (Constraint "name_unique" Unique /\ Constraint (Composite "unique_pairs") Unique)
      , by ∷ Column (Maybe Int) (Constraint (Composite "unique_pairs") Unique)
      )

uniqueValues ∷ Table "unique_values" UniqueValues
uniqueValues = Table

type CompositeT =
      ( id ∷ Column Int (Identity /\ Constraint (Composite "pk_composite") PrimaryKey)
      , second_id ∷ Column Int (Constraint (Composite "pk_composite") PrimaryKey)
      , name ∷ String
      )

composite ∷ Table "composite" CompositeT
composite = Table

id ∷ Proxy "id"
id = Proxy

secondId ∷ Proxy "second_id"
secondId = Proxy

name ∷ Proxy "name"
name = Proxy

surname ∷ Proxy "surname"
surname = Proxy

_by ∷ Proxy "by"
_by = Proxy

test = insert # into uniqueValues (name /\ _by) # values ("mary" /\ Nothing) # on # conflict name # do_ # update # set (_by .=. excluded ... name)
