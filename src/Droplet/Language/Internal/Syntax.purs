-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Syntax (class ToRest, class UnwrapAll, class IsTableAliased, class IsNamedQuery, class IsNamedSubQuery, toRest, class RequiredFields, class ToAs, class ToFrom, class ToInsertFields, class ToInsertValues, class ToPrepare, class ToProjection, class ToSelect, class ToSingleColumn, class ToSubExpression, class ToUpdatePairs, class ToReturning, class ToReturningFields, class ToWhere, class UniqueColumnNames, As(..), Delete(..), E, From(..), Insert(..), OrderBy(..), class ToOrderBy, class ToOrderByFields, class ToLimit, Limit(..), orderBy, Into(..), Plan(..), Prepare(..), Select(..), Returning(..), Set(..), Update(..), class ToExtraFields, Values(..), Where(..), as, delete, asc, desc, Sort(..), from, insert, limit, into, prepare, select, set, update, values, returning, wher)  where

import Droplet.Language.Internal.Condition
import Droplet.Language.Internal.Definition
import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Function (Aggregate)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy)


data E = E


----------------------PREPARE----------------------------

data Prepare q = Prepare q Plan

newtype Plan = Plan String

class ToPrepare (q :: Type)

--to allow general selects/insert, ToQuery would need to check for invalid statements
instance selectToPrepare :: ToPrepare (Select s p (From f fields rest))

instance insertToPrepare :: ToPrepare (Insert (Into name fields fieldNames (Values v rest)))

instance updateToPrepare :: ToPrepare (Update name fields (Set v rest))

instance deleteToPrepare :: ToPrepare (Delete (From f fields rest))

prepare :: forall q. ToPrepare q => Plan -> q -> Prepare q
prepare plan s = Prepare s plan



----------------------SELECT----------------------------

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

data Select s (projection :: Row Type) rest = Select s rest


class ToSelect (s :: Type)

instance fieldToSelect :: ToSelect (Proxy name)

else instance starToSelect :: ToSelect Star

else instance asIntToSelect :: ToSelect (As alias Int)

else instance asFieldToSelect :: ToSelect (As alias (Proxy name))

else instance pathToSelect :: ToSelect (Path table name)

else instance asPathToSelect :: ToSelect (As alias (Path table name))

else instance asAggregateToSelect :: ToSelect (As alias (Aggregate inp fields out))

else instance tupleToSelect :: (ToSelect r, ToSelect t) => ToSelect (r /\ t)

else instance fromFieldsToSelect :: ToSubExpression q => ToSelect q


class ToSubExpression (s :: Type)

--for sub queries only a single column can be returned
instance fromFieldToSubExpression :: ToSubExpression (Select (Proxy name) projection rest)

else instance fromIntToSubExpression :: ToSubExpression (Select (As alias Int) rojection rest)

else instance fromAsFieldToSubExpression :: ToSubExpression (Select (As alias (Proxy name)) projection rest)

else instance pathAsToSubExpression :: ToSubExpression (Select (As alias (Path table name)) projection rest)

else instance pathToSubExpression :: ToSubExpression (Select (Path table name) projection rest)

else instance asAggregateToSubExpression :: ToSubExpression (Select (As alias (Aggregate inp fields out)) projection rest)

else instance fromTupleToSubExpression :: Fail (Text "Subquery must return a single column") => ToSubExpression (Select (a /\ b) projection rest)

else instance asFieldToSubExpression :: (ToSubExpression s, IsNamedQuery rest alias) => ToSubExpression (Select s projection (From f fd rest))


select :: forall s projection. ToSelect s => s -> Select s projection E
select s = Select s E



-------------------------------FROM----------------------------

data From f (fields :: Row Type) rest = From f rest

--from needs to check if columns referenced by Select are valid
class ToFrom (f :: Type) (q :: Type) (fields :: Row Type) | q f -> fields

instance tableToFrom :: (
      ToProjection s fields "" selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Table name fields) (Select s unique E) fields

--named tables like select ... from table as alias
instance tableAsToFrom :: (
      ToProjection s fields alias selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (As alias (Table name fields)) (Select s unique E) fields

instance tableDeleteToFrom :: ToFrom (Table name fields) (Delete E) fields

--named queries like (select ... from ... ) as alias
instance asToFrom :: (
      IsNamedQuery rest alias,
      ToProjection t projection alias selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Select s projection (From f fd rest)) (Select t unique E) fields


from :: forall f q fields sql. ToFrom f q fields => ToRest q (From f fields E) sql => f -> q -> sql
from f q = toRest q $ From f E



-------------------------------WHERE----------------------------

data Where rest = Where Filtered rest


class ToWhere (q :: Type) (fields :: Row Type) | q -> fields

instance selectToWhere :: ToWhere (Select s projection (From f fields E)) fields

instance updateToWhere :: ToWhere (Update name fields (Set v E)) fields

instance deleteToWhere :: ToWhere (Delete (From f fields E)) fields


wher :: forall q fields sql. ToWhere q fields => ToRest q (Where E) sql => Condition fields -> q -> sql
wher (Condition filtered) q = toRest q $ Where filtered E



----------------------------AS----------------------------

--beware of brackets
newtype As (alias :: Symbol) rest = As rest


class ToAs (q :: Type) (alias :: Symbol) | q -> alias

instance intToAs :: ToAs Int alias

instance tableToAs :: ToAs (Table name fields) alias

instance fieldToAs :: ToAs (Proxy name) alias

instance pathToAs :: ToAs (Path table name) alias

instance aggregateToAs :: ToAs (Aggregate inp fields out) alias

instance subQueryFromToAs :: ToAs (Select s p (From f fields rest)) alias

as :: forall q alias sql. ToAs q alias => ToRest q (As alias E) sql => Proxy alias -> q -> sql
as _ q = toRest q $ As E



---------------------------ORDER BY------------------------------------------

data OrderBy f rest = OrderBy f rest

data Sort (name :: Symbol) = Asc | Desc


class ToOrderBy (f :: Type) (q :: Type)

instance fromToOrderBy :: (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields E))

instance whereToOrderBy :: (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields (Where E)))


class ToOrderByFields (f :: Type) (fields :: Row Type) | f -> fields

instance fieldToOrderByFields :: Cons name t e fields => ToOrderByFields (Proxy name) fields

instance sortToOrderByFields :: Cons name t e fields  => ToOrderByFields (Sort name) fields

instance tupleToOrderByFields :: (ToOrderByFields a fields, ToOrderByFields b fields) => ToOrderByFields (a /\ b) fields


--works as long we dont support order by number
asc :: forall name. Proxy name -> Sort name
asc _ = Asc

desc :: forall name. Proxy name -> Sort name
desc _ = Desc

orderBy :: forall f q sql. ToOrderBy f q => ToRest q (OrderBy f E) sql => f -> q -> sql
orderBy f q = toRest q $ OrderBy f E



------------------------LIMIT---------------------------

data Limit rest = Limit Int rest


class ToLimit (q :: Type)

instance fromToLimit :: ToLimit (Select s projection (From fr fields (OrderBy f E)))

instance whereToLimit :: ToLimit (Select s projection (From fr fields (Where (OrderBy f E))))

limit :: forall q sql. ToLimit q => ToRest q (Limit E) sql => Int -> q -> sql
limit n q = toRest q $ Limit n E


------------------------COALESCE---------------------------

--a (special) function, but we have to define it here
-- data Coalesce q projection = Coalesce q

-- class ToCoalesce t projection | t -> projection

-- instance selToCoalesce :: ToProjection (Select s p (From f fields rest)) fields projection => ToCoalesce (Select s p (From f fields rest)) projection

-- instance asIntToCoalesce :: => Cons alias Int () projection => ToCoalesce (As alias Int) projection

-- instance tupleToCoalasce :: (ToCoalesce a projection, ToCoalesce b projection) => ToCoalesce (Tuple a b) projection

-- coalesce :: forall q projection. ToCoalesce q projection => q -> Coalesce q projection
-- coalesce t = Coalesce t



---------------------------INSERT------------------------------------------

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



---------------------------UPDATE------------------------------------------

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



---------------------------DELETE------------------------------------------

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

--real work is done in from and wher
newtype Delete rest = Delete rest

delete :: Delete E
delete = Delete E




---------------------------RETURNING------------------------------------------

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

--can we get rid of fields here?
newtype Returning (fields :: Row Type) f = Returning f


class ToReturning (f :: Type) (fields :: Row Type) (q :: Type) | q -> fields

instance insertToReturning :: ToReturningFields f fields => ToReturning f fields (Insert (Into tn fields fn (Values fv E)))


class ToReturningFields (f :: Type) (fields :: Row Type) | f -> fields

instance fieldToReturningFields :: Cons name t e fields => ToReturningFields (Proxy name) fields

instance tupleToReturningFields :: (ToReturningFields a fields, ToReturningFields b fields) => ToReturningFields (a /\ b) fields


returning :: forall f fields q sql. ToReturning f fields q => ToRest q (Returning fields f) sql => f -> q -> sql
returning f q = toRest q $ Returning f




------------------------Projection machinery---------------------------

-- | Row Type of columns projected by the query
class ToProjection (s :: Type) (fields :: Row Type) (alias :: Symbol) (projection :: Row Type) | s -> fields projection

--simple columns
instance fieldToProjection :: (UnwrapDefinition t u, Cons name t e fields, Cons name u () projection) => ToProjection (Proxy name) fields alias projection

--alias same scope
else instance pathToProjection :: (UnwrapDefinition t u, Cons name t e fields, Append alias "." path, Append path name fullPath, Cons fullPath u () projection) => ToProjection (Path alias name) fields alias projection
--alias outer scope
else instance pathAToProjection :: (Append table "." path, Append path name fullPath, Cons fullPath (Path table name) () projection) => ToProjection (Path table name) fields alias projection

else instance intAsToProjection :: Cons alias Int () projection => ToProjection (As alias Int) fields a projection

else instance aggregateToProjection :: Cons alias t () projection => ToProjection (As alias (Aggregate inp fields t)) fields a projection

else instance fieldAsToProjection :: (UnwrapDefinition t u, Cons name t e fields, Cons alias u () projection) => ToProjection (As alias (Proxy name)) fields a projection

else instance pathAAsToProjection :: (UnwrapDefinition t u, Cons name t e fields, Cons alias u () projection) => ToProjection (As alias (Path table name)) fields table projection

else instance pathAsToProjection :: Cons alias (Path table name) () projection => ToProjection (As alias (Path table name)) fields a projection

else instance starToProjection :: (RowToList fields list, UnwrapAll list projection) => ToProjection Star fields alias projection

else instance tupleToProjection :: (ToProjection s fields alias some, ToProjection t fields alias more, Union some more projection) => ToProjection (s /\ t) fields alias projection

--change projection to Maybe since subqueries may return null
else instance selectFromRestToProjection :: (
      IsTableAliased f table,
      ToProjection s fields table projection,
      RowToList projection list,
      ToSingleColumn list name t,
      IsNamedSubQuery rest name alias, -- if the subquery ends in as
      Cons alias t () single
) => ToProjection (Select s p (From f fields rest)) fd a single

else instance failToProjection :: Fail (Text "Cannot recognize projection") => ToProjection x f a p

--not required but makes for clearer type errors
class ToSingleColumn (fields :: RowList Type) (name :: Symbol) (t :: Type) | fields -> name t

instance singleToSingleColumn :: ToSingleColumn (RL.Cons name (Maybe t) RL.Nil) name (Maybe t)

else instance singleMaybeToSingleColumn :: ToSingleColumn (RL.Cons name t RL.Nil) name (Maybe t)


-- | Query projections should not repeat column names
class UniqueColumnNames (some :: Row Type) (more :: Row Type)

instance sameUniqueColumnNames :: UniqueColumnNames fields fields


class IsTableAliased (f :: Type) (alias :: Symbol) | f -> alias

instance asTableIsTableAliased :: IsTableAliased (As alias (Table name fields)) alias

else instance asSelIsTableAliased :: IsNamedSubQuery rest "" alias => IsTableAliased (Select s p (From f fd rest)) alias

else instance elseIsTableAliased :: IsTableAliased f ""


class IsNamedQuery (q :: Type) (alias :: Symbol) | q -> alias

instance whereIsNamedQuery :: IsNamedQuery rest alias => IsNamedQuery (Where rest) alias

instance orderByIsNamedQuery :: IsNamedQuery rest alias => IsNamedQuery (OrderBy f rest) alias

instance limitIsNamedQuery :: IsNamedQuery rest alias => IsNamedQuery (Limit rest) alias

instance eIsNamedQuery :: Fail (Text "Query in FROM clause must be named") => IsNamedQuery E alias

instance asIsNamedQuery :: IsNamedQuery (As alias E) alias


class IsNamedSubQuery (q :: Type) (name :: Symbol) (alias :: Symbol) | q -> name alias

instance whereIsNamedSubQuery :: IsNamedSubQuery rest name alias => IsNamedSubQuery (Where rest) name alias

instance orderByIsNamedSubQuery :: IsNamedSubQuery rest name alias => IsNamedSubQuery (OrderBy f rest) name alias

instance limitIsNamedSubQuery :: IsNamedSubQuery rest name alias => IsNamedSubQuery (Limit rest) name alias

instance eIsNamedSubQuery :: IsNamedSubQuery E name name

instance asIsNamedSubQuery :: IsNamedSubQuery (As alias E) name alias


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


class UnwrapAll (list :: RowList Type) (projection :: Row Type) | list -> projection

instance nilUnwrapAll :: UnwrapAll RL.Nil ()

instance consUnwrapAll :: (
      UnwrapDefinition t u,
      Cons name u () head,
      UnwrapAll rest tail,
      Union head tail projection
) => UnwrapAll (RL.Cons name t rest) projection


---------------------------Rest machinery------------------------------------------

--this trick does the actual replacement of E for the next statement
-- we could use a alternative encoding for queries (e.g. tuples),
-- but I think this one is clearer (for the end user) when looking at the (final) types
class ToRest a b c | a -> b, a b -> c where
      toRest :: a -> b -> c

instance selectToRest :: ToRest rest b c => ToRest (Select s p rest) b (Select s p c) where
      toRest (Select s rest) b = Select s $ toRest rest b

else instance fromToRest :: ToRest rest b c => ToRest (From f fd rest) b (From f fd c) where
      toRest (From f rest) b = From f $ toRest rest b

else instance whereToRest :: ToRest rest b c => ToRest (Where rest) b (Where c) where
      toRest (Where f rest) b = Where f $ toRest rest b

else instance orderByToRest :: ToRest rest b c => ToRest (OrderBy f rest) b (OrderBy f c) where
      toRest (OrderBy f rest) b = OrderBy f $ toRest rest b

else instance limitByToRest :: ToRest rest b c => ToRest (Limit rest) b (Limit c) where
      toRest (Limit n rest) b = Limit n $ toRest rest b

else instance updateToRest :: ToRest rest b c => ToRest (Update n f rest) b (Update n f c) where
      toRest (Update rest) b = Update $ toRest rest b

else instance insertToRest :: ToRest rest b c => ToRest (Insert rest) b (Insert c) where
      toRest (Insert rest) b = Insert $ toRest rest b

else instance intoToRest :: ToRest rest b c => ToRest (Into n f fd rest) b (Into n f fd c) where
      toRest (Into f rest) b = Into f $ toRest rest b

else instance valuesToRest :: ToRest rest b c => ToRest (Values v rest) b (Values v c) where
      toRest (Values v rest) b = Values v $ toRest rest b

else instance setToRest :: ToRest rest b c => ToRest (Set p rest) b (Set p c) where
      toRest (Set p rest) b = Set p $ toRest rest b

else instance deleteToRest :: ToRest rest b c => ToRest (Delete rest) b (Delete c) where
      toRest (Delete rest) b = Delete $ toRest rest b

else instance asIntToRest :: ToRest (As alias E) b (As alias b) where
      toRest (As _) b = As b

else instance eToRest :: ToRest E b b where
      toRest _ b = b

else instance elseToRest :: ToRest b a c => ToRest a b c where
      toRest a b = toRest b a

