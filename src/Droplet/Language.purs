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
      , Identity
      , NamedConstraint
      , ConstraintDefinition
      , RenamedColumn
      , Column
      , SharedConstraint
      , ForeignKey
      , ColumnDefinition
      , Default(..)
      , PrimaryKey
      , Constraint
      , Unique
      , E
      , Path
      , Star(..)
      , Table(..)
      , fromValue
      , star
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
import Droplet.Language.Internal.Translate (class ToQuery, Query(..))
import Droplet.Language.Internal.Translate as DLIQ
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
      , Where
      , as
      , delete
      , from
      , insert
      , into
      , orderBy
      , join
      , leftJoin
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