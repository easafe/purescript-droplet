-- | Query language
module Droplet.Language (spyQuery, module Exported) where

import Data.Maybe (Maybe)
import Droplet.Language.Internal.Condition
      ( and
      , equals
      , isNotNull
      , notEquals
      , greaterThan
      , lesserThan
      , or
      , not
      , in_
      , Op
      , Not
      , In
      , Exists
      , (.&&.)
      , (.<>.)
      , (.=.)
      , (.>.)
      , (.<.)
      , (.||.)
      ) as Exported
import Droplet.Language.Internal.Definition
      ( class FromValue
      , class ToParameters
      , class ToValue
      , Auto
      , PrimaryKey
      , E
      , Path
      , Unique
      , Default(..)
      , Star(..)
      , Table(..)
      , star
      , fromValue
      , toValue
      , (...)
      ) as Exported
import Droplet.Language.Internal.Function
      ( count
      , coalesce
      , string_agg
      , function
      , function'
      , Aggregate
      , FunctionSignature
      , FunctionSignature'
      , random
      ) as Exported
import Droplet.Language.Internal.Gen (class ToQuery, Query(..))
import Droplet.Language.Internal.Gen as DLIQ
import Droplet.Language.Internal.Syntax
      ( As
      , Inner
      , Outer
      , Delete
      , From
      , Insert
      , Into
      , OrderBy
      , Plan
      , Prepare
      , Select
      , Set
      , Update
      , Returning
      , Limit
      , Values
      , Create
      , Tabl
      , Where
      , as
      , delete
      , tabl
      , from
      , insert
      , into
      , orderBy
      , join
      , leftJoin
      , create
      , groupBy
      , Join
      , Offset
      , offset
      , Union
      , union
      , unionAll
      , On
      , Distinct
      , distinct
      , exists
      , prepare
      , select
      , set
      , asc
      , on
      , desc
      , update
      , limit
      , values
      , wher
      , returning
      ) as Exported
import Droplet.Language.Internal.Syntax (Plan)
import Foreign (Foreign)

-- | Debug generated query
spyQuery ∷
      ∀ q projection.
      ToQuery q projection ⇒
      q →
      { parameters ∷ Array Foreign
      , plan ∷ Maybe Plan
      , query ∷ String
      }
spyQuery q = case DLIQ.buildQuery q of
      Query plan sql parameters → { plan, query: sql, parameters }