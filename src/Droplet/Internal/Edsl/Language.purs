-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
-- |
-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it

module Droplet.Internal.Edsl.Language where

import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Filter
import Prelude

import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)

{-

full select syntax supported by postgresql (https://www.postgresql.org/docs/current/sql-select.html)

[ WITH [ RECURSIVE ] with_query [, ...] ]
SELECT [ ALL | DISTINCT [ ON ( expression [, ...] ) ] ]
    [ * | expression [ [ AS ] output_name ] [, ...] ]
    [ FROM from_item [, ...] ]
    [ WHERE condition ]
    [ GROUP BY grouping_element [, ...] ]
    [ HAVING condition ]
    [ WINDOW window_name AS ( window_definition ) [, ...] ]
    [ { UNION | INTERSECT | EXCEPT } [ ALL | DISTINCT ] select ]
    [ ORDER BY expression [ ASC | DESC | USING operator ] [ NULLS { FIRST | LAST } ] [, ...] ]
    [ LIMIT { count | ALL } ]
    [ OFFSET start [ ROW | ROWS ] ]
    [ FETCH { FIRST | NEXT } [ count ] { ROW | ROWS } { ONLY | WITH TIES } ]
    [ FOR { UPDATE | NO KEY UPDATE | SHARE | KEY SHARE } [ OF table_name [, ...] ] [ NOWAIT | SKIP LOCKED ] [...] ]

where from_item can be one of:

    [ ONLY ] table_name [ * ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
                [ TABLESAMPLE sampling_method ( argument [, ...] ) [ REPEATABLE ( seed ) ] ]
    [ LATERAL ] ( select ) [ AS ] alias [ ( column_alias [, ...] ) ]
    with_query_name [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    [ LATERAL ] function_name ( [ argument [, ...] ] )
                [ WITH ORDINALITY ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    [ LATERAL ] function_name ( [ argument [, ...] ] ) [ AS ] alias ( column_definition [, ...] )
    [ LATERAL ] function_name ( [ argument [, ...] ] ) AS ( column_definition [, ...] )
    [ LATERAL ] ROWS FROM( function_name ( [ argument [, ...] ] ) [ AS ( column_definition [, ...] ) ] [, ...] )
                [ WITH ORDINALITY ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    from_item [ NATURAL ] join_type from_item [ ON join_condition | USING ( join_column [, ...] ) ]

and grouping_element can be one of:

    ( )
    expression
    ( expression [, ...] )
    ROLLUP ( { expression | ( expression [, ...] ) } [, ...] )
    CUBE ( { expression | ( expression [, ...] ) } [, ...] )
    GROUPING SETS ( grouping_element [, ...] )

and with_query is:

    with_query_name [ ( column_name [, ...] ) ] AS [ [ NOT ] MATERIALIZED ] ( select | values | insert | update | delete )

TABLE [ ONLY ] table_name [ * ]

-}

{-

full select syntax supported by droplet

SELECT
      * | column | AS | SELECT | [, ...]
      [ FROM ]
      [ WHERE ]

AS
      integer | FROM output_name | WHERE output_name

FROM
      table_name | AS

WHERE
      { field | parameter } OPERATOR { field | parameter } | [ { and | or } ] | [...]

OPERATOR
      = | <> |

-}

data E = E

----------------------PREPARE----------------------------

data Prepare s = Prepare s Plan

newtype Plan = Plan String

prepare :: forall s projection rest. Plan -> Select s projection rest -> Prepare (Select s projection rest)
prepare plan s = Prepare s plan



----------------------SELECT----------------------------

--the projection is not really needed here but it might help with understanding type errors for the most common queries
data Select s (projection :: Row Type) rest = Select s rest

class ToSelect r s projection | r -> s, r -> projection where
      toSelect :: r -> Select s projection E

--needs more instance for scalars
-- might be nice to able to project parameters too
-- we also dont accept naked selects as subqueries/as/whatever

instance fieldToSelect :: ToSelect (Field name) (Field name) projection where
      toSelect s = Select s E

else instance starToSelect :: ToSelect Star Star projection where
      toSelect s = Select s E

else instance asIntToSelect :: ToSelect (As Int alias) (As Int alias) projection where
      toSelect a = Select a E

else instance asFieldToSelect :: ToSelect (As (Field name) alias) (As (Field name) alias) projection where
      toSelect a = Select a E

else instance tupleToSelect :: (ToSelect r s projection, ToSelect t u projection) => ToSelect (Tuple r t) (Tuple (Select s projection E) (Select u projection E)) projection where
      toSelect (Tuple s t) = Select (Tuple (toSelect s) (toSelect t)) E

else instance fromFieldsToSelect :: ToSubExpression q => ToSelect q q projection where
      toSelect q = Select q E

class ToSubExpression (r :: Type)

--for sub queries only a single column can be returned
instance fromFieldToSubExpression :: ToSubExpression (Select (Field name) projection rest)

else instance fromIntToSubExpression :: ToSubExpression (Select (As Int name) projection rest)

else instance fromAsFieldToSubExpression :: ToSubExpression (Select (As (Field name) alias) projection rest)

else instance fromTupleToSubExpression :: Fail (Text "Subquery must return a single column") => ToSubExpression (Select (Tuple a b) projection rest)

else instance asFieldToSubExpression :: ToSubExpression s => ToSubExpression (Select s projection (As E alias))

select :: forall r s projection. ToSelect r s projection => r -> Select s projection  E
select = toSelect



-------------------------------FROM----------------------------

data From f (fields :: Row Type) rest = From f rest

class ToFrom f s projection fields | f -> s, f -> projection, f -> fields where
      toFrom :: forall p. f -> Select s p E -> Select s projection (From f fields E)

instance tableToFrom :: (
      ToProjection s fields selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Table name fields) s unique fields where
      toFrom table (Select s _) = Select s $ From table E

else instance asToFrom :: (
      ToProjection t fields selected,
      ToProjection s selected projection,
      Nub projection unique,
      UniqueColumnNames projection unique
) => ToFrom (Select (Select t pp (From f fields rest)) p (As E a)) s unique selected where
      toFrom as (Select s _) = Select s $ From as E

--not ideal!
else instance elseToFrom :: Fail (Text "Projection source must be table or aliased subquery") => ToFrom f s p fd where
      toFrom f (Select s _) = Select s (From f E)

from :: forall f s p projection fields. ToFrom f s projection fields => f -> Select s p E -> Select s projection (From f fields E)
from = toFrom



-------------------------------WHERE----------------------------

data Where rest = Where Filtered rest

wher :: forall f s projection fields. Filters fields -> Select s projection (From f fields E) -> Select s projection (From f fields (Where E))
wher (Filters filtered) (Select s (From f E)) = Select s <<< From f $ Where filtered E



----------------------------AS----------------------------

newtype As q (alias :: Symbol) = As q

class ToAs q as name | q -> name, q -> as where
      toAs :: Alias name -> q -> as

instance intToAs :: ToAs Int (As Int name) name where
      toAs _ n = As n

instance fieldToAs :: ToAs (Field name) (As (Field name) alias) alias where
      toAs _ fd = As fd

instance subQueryFromToAs :: ToAs (Select s projection (From f fields rest)) (Select (Select s projection (From f fields rest)) projection (As E name)) name where
      toAs _ s = Select s (As E)

as :: forall q as name. ToAs q as name => Alias name -> q -> as
as a q = toAs a q



------------------------ORDER BY---------------------------




------------------------LIMIT---------------------------




------------------------Projection machinery---------------------------

-- | Row Type of columns projected by the query
class ToProjection (s :: Type) (fields :: Row Type) (projection :: Row Type) | s -> fields, s -> projection

--simple columns
instance fieldToProjection :: (Cons name t e fields, Cons name t () projection) => ToProjection (Field name) fields projection

else instance intAsToProjection :: Cons alias Int () projection => ToProjection (As Int alias) fields projection

else instance fieldAsToProjection :: (Cons name t e fields, Cons alias t () projection) => ToProjection (As (Field name) alias) fields projection

else instance starToProjection :: Union fields () projection => ToProjection Star fields projection

else instance tupleToProjection :: (ToProjection s fields some, ToProjection t fields more, Union some more extra) => ToProjection (Tuple (Select s p sr) (Select t pp tr)) fields extra

--subquery columns
else instance selectFromRestToProjection :: ToProjection s fields projection => ToProjection (Select s p (From f fields rest)) fd projection

else instance asToProjection :: (
      ToProjection s fields extra,
      RowToList extra list,
      ToSingleColumn list t,
      Cons alias t () single
) => ToProjection (Select s p (As E alias)) fields single

else instance failToProjection :: Fail (Text "Cannot recognize projection") => ToProjection x f p


class ToSingleColumn (fields :: RowList Type) (t :: Type) | fields -> t

instance singleToSingleColumn :: ToSingleColumn (RL.Cons name t RL.Nil) t


-- | Query projections should not repeat column names
class UniqueColumnNames (some :: Row Type) (more :: Row Type)

instance sameUniqueColumnNames :: UniqueColumnNames fields fields



{-

full insert syntax supported by postgresql https://www.postgresql.org/docs/current/sql-insert.html

[ WITH [ RECURSIVE ] with_query [, ...] ]
INSERT INTO table_name [ AS alias ] [ ( column_name [, ...] ) ]
    [ OVERRIDING { SYSTEM | USER } VALUE ]
    { DEFAULT VALUES | VALUES ( { expression | DEFAULT } [, ...] ) [, ...] | query }
    [ ON CONFLICT [ conflict_target ] conflict_action ]
    [ RETURNING * | output_expression [ [ AS ] output_name ] [, ...] ]

where conflict_target can be one of:

    ( { index_column_name | ( index_expression ) } [ COLLATE collation ] [ opclass ] [, ...] ) [ WHERE index_predicate ]
    ON CONSTRAINT constraint_name

and conflict_action is one of:

    DO NOTHING
    DO UPDATE SET { column_name = { expression | DEFAULT } |
                    ( column_name [, ...] ) = [ ROW ] ( { expression | DEFAULT } [, ...] ) |
                    ( column_name [, ...] ) = ( sub-SELECT )
                  } [, ...]
              [ WHERE condition ]

-}

{-

full insert syntax supported by droplet

INSERT INTO
      table name fields
      [VALUES { values |  }]

-}

---------------------------INSERT------------------------------------------

-- data InsertInto (name :: Symbol) (fields :: Row Type) fieldNames (parameters :: Row Type) rest = InsertInto (Table name fields) fieldNames rest

-- newtype Values fieldValues = Values fieldValues


-- --need to check if we are missing mandatory fields
-- class ToInsertFields (fields :: Row Type) (fieldNames :: Type) | fieldNames -> fields

-- instance fieldToInsertFields :: Cons name t e fields => ToInsertFields fields (Field name)

-- instance tupleToInsertFields :: (Cons name t e fields, ToInsertFields fields rest) => ToInsertFields fields (Tuple (Field name) rest)

-- insertInto :: forall tableName fields fieldNames parameters. ToInsertFields fields fieldNames => Table tableName fields -> fieldNames -> InsertInto tableName fields fieldNames parameters E
-- insertInto table fieldNames = InsertInto table fieldNames E


-- class ToInsertValues (fields :: Row Type) (fieldNames :: Type) (fieldValues :: Type) (parameters :: Row Type) | fieldNames -> fields, fieldNames -> fieldValues, fieldNames -> parameters

-- instance fieldToInsertValues :: (Cons name t e fields, Cons name t () single) => ToInsertValues fields (Field name) (Parameter n) single

-- else instance tupleToInsertValues :: (
--       Cons name t e fields,
--       Cons name t () head,
--       ToInsertValues fields some more tail,
--       Lacks name tail,
--       Union head tail all
-- ) => ToInsertValues fields (Tuple (Field name) some) (Tuple (Parameter n) more) all

-- values :: forall tableName fields fieldNames fieldValues parameters. ToInsertValues fields fieldNames fieldValues parameters => fieldValues -> InsertInto tableName fields fieldNames parameters E -> InsertInto tableName fields fieldNames parameters (Values fieldValues)
-- values fieldValues (InsertInto table fieldNames _) = InsertInto table fieldNames (Values fieldValues)
