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

prepare :: forall s parameters rest. Plan -> Record parameters -> Select s parameters rest -> Prepare (Select s parameters rest) parameters
prepare plan record s = Prepare s record plan



----------------------SELECT----------------------------

data Select s (parameters :: Row Type) rest = Select s rest

class ToSelect r s parameters | r -> s, r -> parameters where
      toSelect :: r -> Select s parameters E

--needs more instance for scalars
-- might be nice to able to project parameters too
-- we also dont accept naked selects as subqueries/as/whatever

instance fieldToSelect :: ToSelect (Field name) (Field name) parameters where
      toSelect s = Select s E

else instance starToSelect :: ToSelect Star Star parameters where
      toSelect s = Select s E

else instance asIntToSelect :: ToSelect (As Int alias parameters projection) (As Int alias parameters projection) parameters where
      toSelect a = Select a E

else instance asFieldToSelect :: ToSelect (As (Field name) alias parameters projection) (As (Field name) alias parameters projection) parameters where
      toSelect a = Select a E

else instance tupleToSelect :: (ToSelect r s parameters, ToSelect t u parameters) => ToSelect (Tuple r t) (Tuple (Select s parameters E) (Select u parameters E)) parameters where
      toSelect (Tuple s t) = Select (Tuple (toSelect s) (toSelect t)) E

else instance fromFieldsToSelect :: ToSubExpression q parameters => ToSelect q q parameters where
      toSelect q = Select q E

class ToSubExpression (r :: Type) (parameters :: Row Type) | r -> parameters

--for sub queries only a single column can be returned
instance fromFieldToSubExpression :: ToSubExpression (Select (Field name) parameters rest) parameters
instance fromIntToSubExpression :: ToSubExpression (Select (As Int name parameters projection) parameters rest) parameters
instance fromAsFieldToSubExpression :: ToSubExpression (Select (As (Field name) alias parameters projection) parameters rest) parameters
instance asFieldToSubExpression :: ToSubExpression q parameters => ToSubExpression (As q alias parameters projection) parameters
instance fromTupleToSubExpression :: Fail (Text "Subquery must return a single column") => ToSubExpression (Select (Tuple a b) parameters rest) parameters

select :: forall r s parameters. ToSelect r s parameters => r -> Select s parameters E
select = toSelect



-------------------------------FROM----------------------------

data From f (fields :: Row Type) rest = From f rest

--lets try to simplify this too
-- easy to get rid of parameters?
class ToFrom f s parameters fields | f -> s, f -> parameters, f -> fields where
      toFrom :: f -> Select s parameters E -> Select s parameters (From f fields E)

instance tableToFrom :: (
      ToProjection s fields selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Table name fields) s parameters fields where
      toFrom table (Select s _) = Select s $ From table E

else instance asToFrom :: (
      ToProjection s projection selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (As q a parameters projection) s parameters projection where
      toFrom as (Select s _) = Select s $ From as E

--not ideal, should be able to be regular type error
else instance elseToFrom :: Fail (Above (Text "Droplet.ToFrom cannot recognize fields of") (Quote x)) => ToFrom x x p f where
      toFrom f (Select s _) = Select s (From f E)

from :: forall f s parameters fields. ToFrom f s parameters fields => f -> Select s parameters E -> Select s parameters (From f fields E)
from = toFrom



-------------------------------WHERE----------------------------

data Where (has :: IsParameterized) rest = Where Filtered rest

wher :: forall f s has fields parameters. Filters fields parameters has -> Select s parameters (From f fields E) -> Select s parameters (From f fields (Where has E))
wher (Filters filtered) (Select s (From f E)) = Select s <<< From f $ Where filtered E



----------------------------AS----------------------------

--can we get rid of parameters here?
data As q (alias :: Symbol) (parameters :: Row Type) (projection :: Row Type) = As q

--restrict it to fields that were actually selected
class ToAs q name parameters projection | q -> name, q -> parameters, q -> projection where
      toAs :: Alias name -> q -> As q name parameters projection

instance intToAs :: (IsSymbol name, Cons name Int () projection) => ToAs Int name parameters projection where
      toAs _ n =  As n

instance fieldToAs :: ToAs (Field name) alias parameters () where
      toAs _ n =  As n

instance subQueryFromToAs :: ToProjection s fields projection => ToAs (Select s parameters (From f fields rest)) name parameters projection where
      toAs _ q = As q

as :: forall q projection parameters name. ToAs q name parameters projection => Alias name -> q -> As q name parameters projection
as a q = toAs a q



------------------------ORDER BY---------------------------




------------------------LIMIT---------------------------




------------------------Projection machinery---------------------------

-- | Row Type of columns projected by the query
class ToProjection (s :: Type) (fields :: Row Type) (projection :: Row Type) | s -> fields, s -> projection

--simple columns
instance fieldToProjection :: (Cons name t e fields, Cons name t () projection) => ToProjection (Field name) fields projection

else instance intAsToProjection :: ToProjection (As Int alias parameters projection) fields projection

else instance fieldAsToProjection :: (Cons name t e fields, Cons alias t e projection) => ToProjection (As (Field name) alias parameters p) fields projection

else instance starToProjection :: Union fields () projection => ToProjection Star fields projection

else instance tupleToProjection :: (ToProjection s fields some, ToProjection t fields more, Union some more extra) => ToProjection (Tuple (Select s parameters sr) (Select t parameters tr)) fields extra

--subquery columns
else instance selectFromRestToProjection :: ToProjection s fields projection => ToProjection (Select s parameters (From f fields rest)) fd projection

else instance asToProjection :: (
      ToProjection q fields extra,
      RowToList extra list,
      ToSingleColumn list t,
      Cons alias t () single
) => ToProjection (As q alias parameters projection) fields single

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
