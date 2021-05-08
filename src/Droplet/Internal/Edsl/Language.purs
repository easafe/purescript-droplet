-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
-- |
-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it

module Droplet.Internal.Edsl.Language where

import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Filter
import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Quote, Text)

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

--this shouldnt create an actual prepare statement (since it'd require unique names and $n parameters)
-- but rather type check all parameter usage in a single place
-- | Unnamed statements will only employ parameter substitution
-- |
-- | Named statements will result into PREPARE
data Prepare q (parameters :: Row Type) = Prepare q (Record parameters) Plan

data Plan =
      NotNamed |
      Named String

prepare :: forall s projection parameters rest. Plan -> Record parameters -> Select s projection parameters rest -> Prepare (Select s projection parameters rest) parameters
prepare plan record s = Prepare s record plan



----------------------SELECT----------------------------

--the projection is not really needed here but it might help with understanding type errors for the most common queries
data Select s (projection :: Row Type) (parameters :: Row Type) rest = Select s rest

class ToSelect r s projection parameters | r -> s, r -> projection, r -> parameters where
      toSelect :: r -> Select s projection parameters E

--needs more instance for scalars
-- might be nice to able to project parameters too
-- we also dont accept naked selects as subqueries/as/whatever

instance fieldToSelect :: ToSelect (Field name) (Field name) projection parameters where
      toSelect s = Select s E

else instance starToSelect :: ToSelect Star Star projection parameters where
      toSelect s = Select s E

else instance asIntToSelect :: ToSelect (As Int alias p) (As Int alias p) projection parameters where
      toSelect a = Select a E

else instance asFieldToSelect :: ToSelect (As (Field name) alias projection) (As (Field name) alias projection) projection parameters where
      toSelect a = Select a E

else instance tupleToSelect :: (ToSelect r s projection parameters, ToSelect t u projection parameters) => ToSelect (Tuple r t) (Tuple (Select s projection parameters E) (Select u projection parameters E)) projection parameters where
      toSelect (Tuple s t) = Select (Tuple (toSelect s) (toSelect t)) E

else instance fromFieldsToSelect :: ToSubExpression q parameters => ToSelect q q projection parameters where
      toSelect q = Select q E

class ToSubExpression (r :: Type) (parameters :: Row Type) | r -> parameters

--for sub queries only a single column can be returned
instance fromFieldToSubExpression :: ToSubExpression (Select (Field name) projection parameters rest) parameters

else instance fromIntToSubExpression :: ToSubExpression (Select (As Int name projection) projection parameters rest) parameters

else instance fromAsFieldToSubExpression :: ToSubExpression (Select (As (Field name) alias p) projection parameters rest) parameters

else instance fromTupleToSubExpression :: Fail (Text "Subquery must return a single column") => ToSubExpression (Select (Tuple a b) projection parameters rest) parameters

else instance asFieldToSubExpression :: ToSubExpression s parameters => ToSubExpression (Select s projection parameters (As E alias projection)) parameters

select :: forall r s projection parameters. ToSelect r s projection parameters => r -> Select s projection parameters E
select = toSelect



-------------------------------FROM----------------------------

data From f (fields :: Row Type) rest = From f rest

--lets try to simplify this too
-- easy to get rid of parameters?
class ToFrom f s projection parameters fields | f -> s, f -> projection, f -> parameters, f -> fields where
      toFrom :: forall p. f -> Select s p parameters E -> Select s projection parameters (From f fields E)

instance tableToFrom :: (
      ToProjection s fields selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Table name fields) s unique parameters fields where
      toFrom table (Select s _) = Select s $ From table E

else instance asToFrom :: (
      ToProjection t fields selected,
      ToProjection s selected projection,
      Nub projection unique,
      UniqueColumnNames projection unique
) => ToFrom (Select (Select t pp parameters (From f fields rest)) p parameters (As E a pr)) s unique parameters unique where
      toFrom as (Select s _) = Select s $ From as E

--not ideal, should be able to be regular type error
else instance elseToFrom :: Fail (Above (Text "Droplet.ToFrom cannot recognize fields of") (Quote x)) => ToFrom x x g p f where
      toFrom f (Select s _) = Select s (From f E)

from :: forall f s p projection parameters fields. ToFrom f s projection parameters fields => f -> Select s p parameters E -> Select s projection parameters (From f fields E)
from = toFrom



-------------------------------WHERE----------------------------

data Where (has :: IsParameterized) rest = Where Filtered rest

wher :: forall f s projection has fields parameters. Filters fields parameters has -> Select s projection parameters (From f fields E) -> Select s projection parameters (From f fields (Where has E))
wher (Filters filtered) (Select s (From f E)) = Select s <<< From f $ Where filtered E



----------------------------AS----------------------------

newtype As q (alias :: Symbol) (projection :: Row Type) = As q

--restrict it to fields that were actually selected
class ToAs q as name | q -> name, q -> as where
      toAs :: Alias name -> q -> as

instance intToAs :: (IsSymbol name, Cons name Int () projection) => ToAs Int (As Int name projection) name where
      toAs _ n = As n

instance fieldToAs :: ToAs (Field name) (As (Field name) alias ()) alias where
      toAs _ fd = As fd

--wacky, but here we pretend to be always renaming a single column
-- from should do the right thing for named queries
instance subQueryFromToAs :: ToProjection (Select s p parameters (As E name p)) fields projection => ToAs (Select s p parameters (From f fields rest)) (Select (Select s p parameters (From f fields rest)) projection parameters (As E name projection)) name where
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

else instance intAsToProjection :: ToProjection (As Int alias projection) fields projection

else instance fieldAsToProjection :: (Cons name t e fields, Cons alias t () projection) => ToProjection (As (Field name) alias p) fields projection

else instance starToProjection :: Union fields () projection => ToProjection Star fields projection

else instance tupleToProjection :: (ToProjection s fields some, ToProjection t fields more, Union some more extra) => ToProjection (Tuple (Select s p parameters sr) (Select t p parameters tr)) fields extra

--subquery columns
else instance selectFromRestToProjection :: ToProjection s fields projection => ToProjection (Select s p parameters (From f fields rest)) fd projection

else instance asToProjection :: (
      ToProjection s fields extra,
      RowToList extra list,
      ToSingleColumn list t,
      Cons alias t () single
) => ToProjection (Select s p parameters (As E alias projection)) fields single

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

INSERT INTO table

-}

---------------------------INSERT------------------------------------------

data InsertInto (name :: Symbol) (fields :: Row Type) fieldNames = InsertInto (Table name fields) fieldNames

class ToTableFields (fields :: Row Type) (fieldNames :: Type) | fieldNames -> fields

instance fieldToTableFields :: Cons name t e fields => ToTableFields fields (Field name)
instance tupleToTableFields :: (Cons name t e fields, ToTableFields fields rest) => ToTableFields fields (Tuple (Field name) rest)

insertInto :: forall name fields fieldNames. ToTableFields fields fieldNames => Table name fields -> fieldNames -> InsertInto name fields fieldNames
insertInto table fieldNames = InsertInto table fieldNames
