-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Syntax (class RequiredFields, class ToNullableSingleColumn, class ToAs, class ToFrom, class ToInsertFields, class ToInsertValues, class ToPrepare, class ToProjection, class ToSelect, class ToSingleColumn, class ToSubExpression, class ToUpdatePairs, class ToReturning, toReturning, class ToReturningFields, class ToWhere, class UniqueColumnNames, As(..), Delete(..), E, From(..), Insert(..), OrderBy(..), class ToOrderBy, class ToOrderByFields, class ToLimit, toLimit, Limit(..), toOrderBy, orderBy, Into(..), Plan(..), Prepare(..), Select(..), Returning(..), Set(..), Update(..), class ToExtraFields, Values(..), Where(..), as, delete, asc, desc, Sort(..), from, insert, limit, into, prepare, select, set, toAs, toFrom, toSelect, toWhere, update, values, returning, wher)  where

import Droplet.Language.Internal.Condition
import Droplet.Language.Internal.Definition
import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Droplet.Language.Internal.Function (Aggregate)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy)
import Unsafe.Coerce as UC

--type families, i cry every time

data E = E

----------------------PREPARE----------------------------

data Prepare q = Prepare q Plan

newtype Plan = Plan String

class ToPrepare (q :: Type)

--to allow general selects/insert, ToQuery would need to check for invalid statements
instance selectToPrepare :: ToPrepare (Select s p (From f fields extra rest))

instance insertToPrepare :: ToPrepare (Insert (Into name fields fieldNames (Values v rest)))

instance updateToPrepare :: ToPrepare (Update name fields (Set v rest))

instance deleteToPrepare :: ToPrepare (Delete fields (From f fields extra rest))

prepare :: forall q. ToPrepare q => Plan -> q -> Prepare q
prepare plan s = Prepare s plan



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
      [ ORDER BY ]
      [ LIMIT ]

AS
      integer | FROM output_name | WHERE output_name

FROM
      table_name | AS

WHERE
      { field | parameter } OPERATOR { field | parameter } | [ { and | or } ] | [...]

OPERATOR
      = | <> |

ORDER BY
      field { ASC | DESC } | [, ...]

LIMIT

-}


----------------------SELECT----------------------------

data Select s (projection :: Row Type) rest = Select s rest

class ToSelect r  where
      toSelect :: forall projection. r -> Select r projection E

--needs more instance for scalars
-- might be nice to able to project parameters too
-- we also dont accept naked selects as subqueries/as/whatever

instance fieldToSelect :: ToSelect (Proxy name) where
      toSelect s = Select s E

else instance starToSelect :: ToSelect Star where
      toSelect s = Select s E

else instance dotToSelect :: ToSelect (Dot name) where
      toSelect s = Select s E

else instance asIntToSelect :: ToSelect (As Int alias) where
      toSelect a = Select a E

else instance asFieldToSelect :: ToSelect (As (Proxy name) alias) where
      toSelect a = Select a E

else instance asDotToSelect :: ToSelect (As (Dot name) alias) where
      toSelect a = Select a E

else instance asAggregateToSelect :: ToSelect (As (Aggregate inp fields out) alias) where
      toSelect a = Select a E

else instance tupleToSelect :: (ToSelect r, ToSelect t) => ToSelect (r /\ t) where
      toSelect (s /\ t) = Select (s /\ t) E

else instance fromFieldsToSelect :: ToSubExpression q => ToSelect q where
      toSelect q = Select q E

class ToSubExpression (r :: Type)

--for sub queries only a single column can be returned
instance fromFieldToSubExpression :: ToSubExpression (Select (Proxy name) projection rest)

else instance fromIntToSubExpression :: ToSubExpression (Select (As Int name) projection rest)

else instance dotToSubExpression :: ToSubExpression (Select (Dot name) projection rest)

else instance fromAsFieldToSubExpression :: ToSubExpression (Select (As (Proxy name) alias) projection rest)

else instance fromAsDotToSubExpression :: ToSubExpression (Select (As (Dot name) alias) projection rest)

else instance asAggregateToSubExpression :: ToSubExpression (Select (As (Aggregate inp fields out) alias) projection rest)

else instance fromTupleToSubExpression :: Fail (Text "Subquery must return a single column") => ToSubExpression (Select (a /\ b) projection rest)

else instance asFieldToSubExpression :: ToSubExpression s => ToSubExpression (Select s projection (As E alias))

select :: forall s projection. ToSelect s => s -> Select s projection  E
select = toSelect



-------------------------------FROM----------------------------

data From f (fields :: Row Type) (extra :: Row Type) rest = From f rest

class ToFrom f q r (extra :: Row Type) | q -> r, r -> extra where
      toFrom :: f -> q -> r

instance tableToFrom :: (
      ToProjection s fields extra selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Table name fields) (Select s p E) (Select s unique (From (Table name fields) fields extra E)) extra where
      toFrom table (Select s _) = Select s $ From table E

else instance tableDeleteToFrom :: ToFrom (Table name fields) (Delete f E) (Delete fields (From (Table name fields) fields () E)) () where
      toFrom table (Delete _) = Delete $ From table E

else instance asToFrom :: (
      ToProjection t fields extra selected,
      ToProjection s selected () projection,
      Nub projection unique,
      UniqueColumnNames projection unique
) => ToFrom (Select (Select t p (From f fields extra rest)) pp (As E a)) (Select s ppp E) (Select s unique (From (Select (Select t p (From f fields extra rest)) pp (As E a)) selected () E)) () where
      toFrom as (Select s _) = Select s $ From as E

--not ideal!
else instance elseToFrom :: Fail (Text "Projection source must be table or aliased subquery") => ToFrom f q r extra where
      toFrom _ _ = UC.unsafeCoerce 23

from :: forall f q r extra. ToFrom f q r extra => f -> q -> r
from = toFrom



-------------------------------WHERE----------------------------

data Where rest = Where Filtered rest

class ToWhere q w fields | q -> w fields where
      toWhere :: Condition fields -> q -> w

--are we missing AS here?
instance selectToWhere :: ToWhere (Select s projection (From f fields extra E)) (Select s projection (From f fields extra (Where E))) fields where
      toWhere (Condition filtered) (Select s (From f E)) = Select s <<< From f $ Where filtered E

else instance updateToWhere :: ToWhere (Update name fields (Set v E)) (Update name fields (Set v (Where E))) fields where
      toWhere (Condition filtered) (Update (Set v E)) = Update <<< Set v $ Where filtered E

else instance deleteToWhere :: ToWhere (Delete fields (From f fields extra E)) (Delete fields (From f fields extra (Where E))) fields where
      toWhere (Condition filtered) (Delete (From f E)) = Delete <<< From f $ Where filtered E

else instance elseToWhere :: Fail (Text "Only full SELECT, UPDATE and DELETE statements can use WHERE clauses") => ToWhere q w fields where
      toWhere _ _ = UC.unsafeCoerce 23

wher :: forall q w fields. ToWhere q w fields => Condition fields -> q -> w
wher conditions q = toWhere conditions q



----------------------------AS----------------------------

newtype As q (alias :: Symbol) = As q

class ToAs q as (name :: Symbol) | q -> name as where
      toAs :: Proxy name -> q -> as

instance intToAs :: ToAs Int (As Int name) name where
      toAs _ n = As n

instance fieldToAs :: ToAs (Proxy name) (As (Proxy name) alias) alias where
      toAs _ fd = As fd

instance dotToAs :: ToAs (Dot name) (As (Dot name) alias) alias where
      toAs _ fd = As fd

instance aggregateToAs :: ToAs (Aggregate inp fields out) (As (Aggregate inp fields out) alias) alias where
      toAs _ fd = As fd

--not ideal but saves work a lot of extra instances
-- and puts As closer to where ToProjection needs it
instance subQueryFromToAs :: (RowToList fields list, ToExtraFields list name extra, ToProjection s fields extra projection) => ToAs (Select s p (From f fields e rest)) (Select (Select s projection (From f fields extra rest)) projection (As E name)) name where
      toAs _ (Select s (From f rest)) = Select (Select s (From f rest)) (As E)

as :: forall q as name. ToAs q as name => Proxy name -> q -> as
as a q = toAs a q


class ToExtraFields (list :: RowList Type) (alias :: Symbol) (extra :: Row Type) | list alias -> extra

instance nilToExtraFields :: ToExtraFields RL.Nil alias ()

instance consToExtraFields :: (
      Append alias "." path,
      Append path name fullPath,
      UnwrapDefinition t u,
      Cons fullPath u () head,
      ToExtraFields rest alias tail,
      Lacks fullPath tail,
      Union head tail all
) => ToExtraFields (RL.Cons name t rest) alias all


---------------------------ORDER BY------------------------------------------

data OrderBy f (fields :: Row Type) rest = OrderBy f rest

data Sort (name :: Symbol) = Asc | Desc

class ToOrderBy f q r | q -> r where
      toOrderBy :: f -> q -> r

instance fromToOrderBy :: (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields extra E)) (Select s projection (From fr fields extra (OrderBy f fields E))) where
      toOrderBy f (Select s (From fr E)) = Select s <<< From fr $ OrderBy f E

else instance whereToOrderBy :: (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields extra (Where E))) (Select s projection (From fr fields extra (Where (OrderBy f fields E)))) where
      toOrderBy f (Select s (From fr (Where fl E))) = Select s <<< From fr <<< Where fl $ OrderBy f E

else instance elseToOrderBy :: Fail (Text "ORDER BY can only follow FROM, WHERE or GROUP BY") => ToOrderBy f q r where
      toOrderBy _ _ = UC.unsafeCoerce 44

class ToOrderByFields (f :: Type) (fields :: Row Type) | f -> fields

instance fieldToOrderByFields :: Cons name t e fields => ToOrderByFields (Proxy name) fields

instance sortToOrderByFields :: Cons name t e fields  => ToOrderByFields (Sort name) fields

instance tupleToOrderByFields :: (ToOrderByFields a fields, ToOrderByFields b fields) => ToOrderByFields (a /\ b) fields

--works as long we dont support order by number
asc :: forall name. Proxy name -> Sort name
asc _ = Asc

desc :: forall name. Proxy name -> Sort name
desc _ = Desc

orderBy :: forall fields q r. ToOrderBy fields q r => fields -> q -> r
orderBy fields q = toOrderBy fields q



------------------------LIMIT---------------------------

--we basically have to copy order by here :(
data Limit rest = Limit Int rest


class ToLimit q r | q -> r where
      toLimit :: Int -> q -> r

instance fromToLimit :: ToLimit (Select s projection (From fr fields extra (OrderBy f fields E))) (Select s projection (From fr fields extra (OrderBy f fields (Limit E)))) where
      toLimit n (Select s (From fr (OrderBy f E))) = Select s <<< From fr <<< OrderBy f $ Limit n E

else instance whereToLimit :: ToLimit (Select s projection (From fr fields extra (Where (OrderBy f fields E)))) (Select s projection (From fr fields extra (Where (OrderBy f fields (Limit E))))) where
      toLimit n (Select s (From fr (Where fl (OrderBy f E)))) = Select s <<< From fr <<< Where fl <<< OrderBy f $ Limit n E

else instance elseToLimit :: Fail (Text "LIMIT can only follow ORDER BY") => ToLimit q r where
      toLimit _ _ = UC.unsafeCoerce 45

limit :: forall q r. ToLimit q r => Int -> q -> r
limit n q = toLimit n q


------------------------COALESCE---------------------------

--a (special) function, but we have to define it here
-- data Coalesce q projection = Coalesce q

-- class ToCoalesce t projection | t -> projection

-- instance selToCoalesce :: ToProjection (Select s p (From f fields rest)) fields projection => ToCoalesce (Select s p (From f fields rest)) projection

-- instance asIntToCoalesce :: => Cons alias Int () projection => ToCoalesce (As Int alias) projection

-- instance tupleToCoalasce :: (ToCoalesce a projection, ToCoalesce b projection) => ToCoalesce (Tuple a b) projection

-- coalesce :: forall q projection. ToCoalesce q projection => q -> Coalesce q projection
-- coalesce t = Coalesce t

------------------------Projection machinery---------------------------

-- | Row Type of columns projected by the query
class ToProjection (s :: Type) (fields :: Row Type) (extra :: Row Type) (projection :: Row Type) | s -> fields projection

--simple columns
instance fieldToProjection :: (UnwrapDefinition t u, Cons name t e fields, Cons name u () projection) => ToProjection (Proxy name) fields extra projection

-- else instance dotToProjection :: (UnwrapDefinition t u, Cons name t e extra, Cons name u () projection) => ToProjection (Dot name) fields extra projection

else instance dotToProjection :: (Cons name t e extra, Cons name t () projection) => ToProjection (Dot name) fields extra projection

else instance intAsToProjection :: Cons alias Int () projection => ToProjection (As Int alias) fields extra projection

else instance aggregateToProjection :: (Cons alias t () projection) => ToProjection (As (Aggregate inp fields t) alias) fields extra projection

else instance fieldAsToProjection :: (UnwrapDefinition t u, Cons name t e fields, Cons alias u () projection) => ToProjection (As (Proxy name) alias) fields extra projection

else instance dotAsToProjection :: (Cons name t e extra, Cons alias t () projection) => ToProjection (As (Dot name) alias) fields extra projection

else instance starToProjection :: Union fields () projection => ToProjection Star fields extra projection

else instance tupleToProjection :: (ToProjection s fields e some, ToProjection t fields e more, Union some more extra) => ToProjection (s /\ t) fields e extra

--change projection to Maybe since subqueries may return null
else instance selectFromRestToProjection :: (
      ToProjection s fields ex extra,
      RowToList extra list,
      ToSingleColumn list name t,
      ToNullableSingleColumn t u,
      Cons name u () single
) => ToProjection (Select s p (From f fields e rest)) fd ex single

--rename projection to alias
else instance asToProjection :: (
      ToProjection s fields ex extra,
      RowToList extra list,
      ToSingleColumn list name t,
      Cons alias t () single
) => ToProjection (Select s p (As E alias)) fields e single

else instance failToProjection :: Fail (Text "Cannot recognize projection") => ToProjection x f e p

--not required but makes for clearer type errors
class ToSingleColumn (fields :: RowList Type) (name :: Symbol) (t :: Type) | fields -> name t

instance singleToSingleColumn :: ToSingleColumn (RL.Cons name t RL.Nil) name t


class ToNullableSingleColumn (t :: Type) (u :: Type) | t -> u

instance maybeToNullableSingleColumn :: ToNullableSingleColumn (Maybe t) (Maybe t)

else instance elseToNullableSingleColumn :: ToNullableSingleColumn t (Maybe t)


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
      [VALUES values ]

-}

---------------------------INSERT------------------------------------------

newtype Insert rest = Insert rest

data Into (name :: Symbol) (fields :: Row Type) fieldNames rest = Into fieldNames rest

data Values fieldValues rest = Values fieldValues rest


class ToInsertFields (fields :: Row Type) (fieldNames :: Type) (inserted :: Row Type) | fieldNames -> fields inserted

instance fieldToInsertFields :: (InvalidField t, Cons name t e fields, Cons name t () single) => ToInsertFields fields (Proxy name) single

instance tupleToInsertFields :: (
      ToInsertFields fields f head,
      ToInsertFields fields rest tail,
      Union head tail all
) => ToInsertFields fields  (f /\ rest) all


class RequiredFields (fieldList :: RowList Type) (required :: Row Type) | fieldList -> required

instance nilRequiredFields :: RequiredFields RL.Nil ()

instance autoCons :: RequiredFields rest required => RequiredFields (RL.Cons n (Auto t) rest) required

else instance defaultCons :: RequiredFields rest required => RequiredFields (RL.Cons n (Default t) rest) required

else instance maybeCons :: RequiredFields rest required => RequiredFields (RL.Cons n (Maybe t) rest) required

else instance elseCons :: (RequiredFields rest tail, Cons name t () head, Lacks name tail, Union head tail required) => RequiredFields (RL.Cons name t rest) required


class ToInsertValues (fields :: Row Type) (fieldNames :: Type) (t :: Type) | fieldNames -> fields t

instance fieldToInsertValues :: (UnwrapDefinition t u, Cons name t e fields, ToValue u) => ToInsertValues fields (Proxy name) u

else instance tupleToInsertValues :: (ToInsertValues fields name value, ToInsertValues fields some more) => ToInsertValues fields (name /\ some) (value /\ more)


insert :: Insert E
insert = Insert E

--as it is, error messages are not intuitive at all
into :: forall tableName fields fieldNames fieldList required e inserted.
      RowToList fields fieldList =>
      RequiredFields fieldList required =>
      ToInsertFields fields fieldNames inserted =>
      Union required e inserted =>
      Table tableName fields -> fieldNames -> Insert E -> Insert (Into tableName fields fieldNames E)
into _ fieldNames _ = Insert (Into fieldNames E)

values :: forall tableName fields fieldNames fieldValues. ToInsertValues fields fieldNames fieldValues => fieldValues -> Insert (Into tableName fields fieldNames E) -> Insert (Into tableName fields fieldNames (Values fieldValues E))
values fieldValues (Insert (Into fieldNames _)) = Insert <<< Into fieldNames $ Values fieldValues E



{-

full update syntax supported by postgresql https://www.postgresql.org/docs/current/sql-update.html

[ WITH [ RECURSIVE ] with_query [, ...] ]
UPDATE [ ONLY ] table_name [ * ] [ [ AS ] alias ]
    SET { column_name = { expression | DEFAULT } |
          ( column_name [, ...] ) = [ ROW ] ( { expression | DEFAULT } [, ...] ) |
          ( column_name [, ...] ) = ( sub-SELECT )
        } [, ...]
    [ FROM from_item [, ...] ]
    [ WHERE condition | WHERE CURRENT OF cursor_name ]
    [ RETURNING * | output_expression [ [ AS ] output_name ] [, ...] ]

-}

{-

full update syntax supported by droplet

UPDATE table name
      SET field = value | [, ...]
      [WHERE conditions]
-}

---------------------------UPDATE------------------------------------------

newtype Update (name :: Symbol) (fields :: Row Type) rest = Update rest

data Set pairs rest = Set pairs rest


class ToUpdatePairs (fields :: Row Type) (pairs :: Type)

instance tupleToUpdatePairs :: (
      InvalidField t,
      UnwrapDefinition t u,
      ToValue u,
      Cons name t e fields
) => ToUpdatePairs fields (Proxy name /\ u)

else instance tupleTuplesToUpdatePairs :: (
      ToUpdatePairs fields head,
      ToUpdatePairs fields tail
) => ToUpdatePairs fields (head /\ tail)


update :: forall name fields. Table name fields -> Update name fields E
update _ = Update E

set :: forall name fields pairs. ToUpdatePairs fields pairs => pairs -> Update name fields E -> Update name fields (Set pairs E)
set pairs (Update _) = Update $ Set pairs E



{-

full delete syntax supported by postgresql https://www.postgresql.org/docs/current/sql-insert.html

[ WITH [ RECURSIVE ] with_query [, ...] ]
DELETE FROM [ ONLY ] table_name [ * ] [ [ AS ] alias ]
    [ USING from_item [, ...] ]
    [ WHERE condition | WHERE CURRENT OF cursor_name ]
    [ RETURNING * | output_expression [ [ AS ] output_name ] [, ...] ]

-}

{-

full update syntax supported by droplet

DELETE FROM table name
      [WHERE condition]
-}

---------------------------DELETE------------------------------------------

--real work is done in from and wher
newtype Delete (fields :: Row Type) rest = Delete rest

delete :: forall fields. Delete fields E
delete = Delete E



{-

https://www.postgresql.org/docs/current/dml-returning.html

{ INSERT | UPDATE | DELETE } RETURNING
      field [AS alias] | [, ...]
      star
-}

{-

full RETURNING syntax supported by droplet

{ INSERT } RETURNING
      field | [, ...]
-}


---------------------------RETURNING------------------------------------------

newtype Returning (fields :: Row Type) f = Returning f

class ToReturning f q r | q -> r where
      toReturning :: f -> q -> r

instance insertToReturning :: ToReturningFields f fields => ToReturning f (Insert (Into tn fields fn (Values fv E))) (Insert (Into tn fields fn (Values fv (Returning fields f)))) where
      toReturning f (Insert (Into fieldNames (Values values E))) = Insert (Into fieldNames (Values values (Returning f)))

else instance elseToReturning :: Fail (Text "RETURNING can only follow INSERT, UPDATE or DELETE") => ToReturning f q r where
      toReturning _ _ = UC.unsafeCoerce 3

class ToReturningFields (f :: Type) (fields :: Row Type) | f -> fields

instance fieldToReturningFields :: Cons name t e fields => ToReturningFields (Proxy name) fields

instance tupleToReturningFields :: (ToReturningFields a fields, ToReturningFields b fields) => ToReturningFields (a /\ b) fields

returning :: forall fields q r. ToReturning fields q r => fields -> q -> r
returning = toReturning

