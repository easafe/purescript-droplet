-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Syntax (class ToRest, class UnwrapAll, class IsTableAliased, class ToPath, class QueryMustBeAliased, class UniqueAliases, class OnCondition, class IsNamedSubQuery, class ToJoin, class OnComparision, class AppendPath, Join(..), Side, Inner, Outer, join, leftJoin, toRest, class ValidGroupByProjection, class GroupByFields, class ToGroupBy, class ToOuterFields, class RequiredFields, class ToAs, class ToFrom, class GroupBySource, class InsertList, class InsertValues, class ToPrepare, class ToProjection, class ToSelect, class ToSingleColumn, class ToSubExpression, class ToUpdatePairs, class ToReturning, class ToReturningFields, class AliasedFields, on, On(..), class ToWhere, class JoinedToMaybe, class UniqueColumnNames, As(..), Delete(..), E, From(..), Insert(..), OrderBy(..), class ToOrderBy, class ToOrderByFields, class ToLimit, Limit(..), groupBy, GroupBy(..), orderBy, Into(..), Plan(..), Prepare(..), Select(..), Returning(..), Set(..), Update(..), Values(..), Where(..), as, delete, asc, desc, Sort(..), from, insert, limit, into, prepare, select, set, update, values, returning, wher)  where

import Droplet.Language.Internal.Definition
import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Condition (class ToCondition, Op)
import Droplet.Language.Internal.Function (Aggregate)
import Droplet.Language.Internal.Keyword (Dot)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy)



----------------------PREPARE----------------------------

data Prepare q = Prepare q Plan

-- | Name of this prepared statement
newtype Plan = Plan String


-- | Only complete statements are accepted by PREPARE
class ToPrepare (q :: Type)

instance ToPrepare (Select s p (From f fields rest))

instance ToPrepare (Insert (Into name fields fieldNames (Values v rest)))

instance ToPrepare (Update name fields (Set v rest))

instance ToPrepare (Delete (From f fields rest))


-- | https://www.postgresql.org/docs/current/sql-prepare.html
-- |
-- | PREPARE statements can be employed to reuse execution plans, and thus optimize performance
-- |
-- | Note: droplet always creates server-side parameters for literal values in queries. In the case of PREPARE, however, literals will be be parsed as statement parameters
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
      [ JOIN ]
      [ WHERE ]
      [ ORDER BY ]
      [ LIMIT ]

AS
      integer | FROM output_name | WHERE output_name

FROM
      table_name | AS

JOIN
     AS ON { field | parameter } OPERATOR { field | parameter } | [ { and | or } ] | [...]

WHERE
      { field | parameter } OPERATOR { field | parameter } | [ { and | or } ] | [...]

OPERATOR
      = | <> |

ORDER BY
      field { ASC | DESC } | [, ...]

LIMIT
      number


-}

-- | SELECT representation. `projection` refers to the final output of this statement
data Select s (projection :: Row Type) rest = Select s rest


-- | Acceptable column type for SELECT statements
class ToSelect (s :: Type)

instance ToSelect (Proxy name)

else instance ToSelect (Path table name)

else instance ToSelect (As alias Int)

else instance ToSelect (As alias (Proxy name))

else instance ToSelect (As alias (Path table name))

else instance ToSelect (As alias (Aggregate inp fields out))

else instance (ToSelect r, ToSelect t) => ToSelect (r /\ t)

else instance ToSelect Star

else instance ToSubExpression q => ToSelect q


-- | Only single columns can be projected by subqueries
-- |
-- | Note: column subqueries may not return a value, thus their projection will be `Maybe` unless the original column type is already `Maybe`
class ToSubExpression (s :: Type)

instance ToSubExpression (Select (Proxy name) projection rest)

instance ToSubExpression (Select (Path table name) projection rest)

instance ToSubExpression (Select (As alias Int) rojection rest)

instance ToSubExpression (Select (As alias (Proxy name)) projection rest)

instance ToSubExpression (Select (As alias (Path table name)) projection rest)

instance ToSubExpression (Select (As alias (Aggregate inp fields out)) projection rest)

instance Fail (Text "Subquery must return a single column") => ToSubExpression (Select (a /\ b) projection rest)


-- | SELECT can project literals, columns and subqueries with the following considerations:
-- |
-- | - Multiple columns are represented by tuples. `Data.Tuple.Nested./\` is convenient for this
-- | - Literal values (e.g., numbers) must be aliased (with AS)
-- | - Columns names in projections must be unique, or referenced by different table aliases (e.g., u.name, t.name)
-- | - Subqueries must return a single column
select :: forall s projection. ToSelect s => s -> Select s projection E
select s = Select s E



-------------------------------FROM----------------------------

data From f (fields :: Row Type) rest = From f rest


-- | Acceptable sources for FROM statements
class ToFrom (f :: Type) (q :: Type) (fields :: Row Type) | q f -> fields

-- | FROM table
instance (
      ToProjection s fields Empty selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Table name fields) (Select s unique E) fields

-- FROM table AS alias
instance (
      ToProjection s fields alias selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (As alias (Table name fields)) (Select s unique E) fields

-- | (DELETE) FROM table
instance ToFrom (Table name fields) (Delete E) fields

-- | FROM (SELECT ... FROM ...) AS alias
instance (
      QueryMustBeAliased rest alias,
      ToProjection s projection alias selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Select t projection (From f fd rest)) (Select s unique E) projection

-- | FROM ... INNER JOIN ...
instance (
      ToProjection s fields Inner selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Join Inner fields l r (On c rest)) (Select s unique E) fields

-- | FROM ... OUTER JOIN ...
instance (
      ToProjection s fields Outer selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Join Outer fields l r (On c rest)) (Select s unique E) fields


-- | FROM accepts the following sources
-- |
-- | - Tables
-- | - Inner and outer joins
-- | - Aliased tables
-- | - Aliased SELECT statements
-- |
-- | Due to how SQL binding works, joins and subqueries require brackets to be parsed correctly. For example:
-- | - `SELECT column FROM (SELECT column FROM table) AS alias` should be `select column # from (select column # from table # as alias)`)
-- | - `SELECT column FROM table alias JOIN other_table other_alias` should be `select column # from ((table # as alias) `join` (other_table # as other_alias))`)
-- |
-- | To aid composition, SELECT projections are only validated on FROM
from :: forall f q fields sql. ToFrom f q fields => ToRest q (From f fields E) sql => f -> q -> sql
from f q = toRest q $ From f E



-------------------------------JOIN----------------------------

-- | Kind for OUTER and INNER joins
data Side

foreign import data Inner :: Side
foreign import data Outer :: Side

data Join (k :: Side) (fields :: Row Type) q r rest = Join q r rest

data On c rest = On c rest


-- | Given a source `q`, compute its (aliased) fields
class ToJoin (q :: Type) (aliased :: Row Type) | q -> aliased

-- | Aliased tables
instance (RowToList fields list, AliasedFields list alias aliased) => ToJoin (As alias (Table name fields)) aliased

-- | Aliased subqueries
instance (
      QueryMustBeAliased rest alias,
      RowToList projection list,
      AliasedFields list alias aliased
) => ToJoin (Select s projection (From f fields rest)) aliased

-- | JOIN ... ON
instance ToJoin (Join k fields l r (On c rest)) fields

--refactor: see if we actually need this when going over toprojection
-- | OUTER JOINs make one side nullable, as a corresponding record may not be found
-- |
-- | For ease of use, this class marks the nullable side fields with `Joined`, later on `ToProjection` will flatten it to `Maybe`
class ToOuterFields (list :: RowList Type) (fields :: Row Type) | list -> fields

instance ToOuterFields RL.Nil ()

-- | Avoid nesting `Joined`s
instance (
      Cons name (Joined t) () head,
      ToOuterFields rest tail,
      Union head tail all
) => ToOuterFields (RL.Cons name (Joined t) rest) all

else instance (
      Cons name (Joined t) () head,
      ToOuterFields rest tail,
      Union head tail all
) => ToOuterFields (RL.Cons name t rest) all


-- | INNER JOIN
-- |
-- | JOIN sources are the same as FROM, with the exception that tables must be aliased
join :: forall r l right left all fields.
      ToJoin l left =>
      ToJoin r right =>
      Union right left all =>
      Nub all fields =>
      UniqueAliases all fields =>
      l -> r -> Join Inner all l r E
join l r = Join l r E

-- | LEFT OUTER JOIN
-- |
-- | JOIN sources are the same as FROM, with the exception that tables must be aliased
leftJoin :: forall r l list out right left all fields.
      ToJoin l left =>
      ToJoin r right =>
      RowToList right list =>
      ToOuterFields list out =>
      Union left out all =>
      Nub all fields =>
      UniqueAliases all fields =>
      l -> r -> Join Outer fields l r E
leftJoin l r = Join l r E

--refactor: check this together with ToCondiction from Condition.purs
class OnCondition (c :: Type) (fields :: Row Type)

instance (OnCondition (Op a b) fields, OnCondition (Op c d) fields) => OnCondition (Op (Op a b) (Op c d)) fields

else instance OnComparision a b fields => OnCondition (Op a b) fields


--only allowing alias.field for now
class OnComparision (a :: Type) (b :: Type) (fields :: Row Type) | a b -> fields

instance (
      UnwrapDefinition t u,
      UnwrapDefinition r u,
      AppendPath alias name fullPath,
      Cons fullPath t d fields,
      AppendPath otherAlias otherName otherFullPath,
      Cons otherFullPath r e fields
) => OnComparision (Path alias name) (Path otherAlias otherName) fields

-- | JOIN ... ON
on :: forall k l r c fields. OnCondition c fields => c -> Join k fields l r E -> Join k fields l r (On c E)
on c (Join q r _) = Join q r $ On c E



-------------------------------WHERE----------------------------

data Where c rest = Where c rest


-- | WHERE can only follow FROM, UPDATE and DELETE
class ToWhere (c :: Type) (q :: Type)
--refactor: examine tocondition after toprojection and thinking more about Path
instance ToCondition c fields alias => ToWhere c (Select s projection (From (As alias f) fields E))

instance ToCondition c fields Empty => ToWhere c (Select s projection (From (Table name fields) fields E))

instance (QueryMustBeAliased rest alias, ToCondition c fields alias) => ToWhere c (Select s projection (From (Select t p (From f fd rest)) fields E))

instance ToCondition c fields Empty => ToWhere c (Update name fields (Set v E))

instance ToCondition c fields Empty => ToWhere c (Delete (From f fields E))


-- | WHERE statement
wher :: forall c q sql. ToWhere c q => ToRest q (Where c E) sql => c -> q -> sql
wher c q = toRest q $ Where c E



----------------------------GROUP BY----------------------------

data GroupBy f rest = GroupBy f rest


-- | GROUP BY can only follow FROM or WHERE
class ToGroupBy (q :: Type) (s :: Type) (fields :: Row Type) | q -> s fields

instance GroupBySource f fields => ToGroupBy (Select s p (From f fd E)) s fields

instance GroupBySource f fields => ToGroupBy (Select s p (From f fd (Where cond E))) s fields

--refactor: this can be made simpler
-- also has a bug, check: select (t ... id) # from (select id # from users # as t) # groupBy (t ... id)
class GroupBySource (f :: Type) (fields :: Row Type) | f -> fields
--refactor: there are many places that inspect from contents to get fields, abstract this
instance GroupBySource (Table name fields) fields

instance (
      RowToList fields list,
      AliasedFields list alias aliased,
      Union aliased fields all
) => GroupBySource (As alias (Table name fields)) all

instance GroupBySource (Join k fields q r rest) fields

instance GroupBySource (Select s projection (From f fd rest)) projection


class GroupByFields (f :: Type) (fields :: Row Type) (grouped :: Row Type) | f -> fields grouped

instance (Cons name t e fields, Cons name t () grouped) => GroupByFields (Proxy name) fields grouped

instance (
      AppendPath alias name fullPath,
      Cons fullPath t e fields,
      Cons fullPath t () g,
      Cons name t g grouped
) => GroupByFields (Path alias name) fields grouped

instance (
      GroupByFields a fields some,
      GroupByFields b fields more,
      Union some more grouped
) => GroupByFields (a /\ b) fields grouped


-- | Asserts that a SELECT ... GROUP BY projection contains only grouped columns or aggreagate functions
class ValidGroupByProjection (s :: Type) (grouped :: Row Type) | s -> grouped

instance Cons name t e grouped => ValidGroupByProjection (Proxy name) grouped

else instance Cons name t e grouped => ValidGroupByProjection (As alias (Proxy name)) grouped

else instance (ValidGroupByProjection a grouped, ValidGroupByProjection b grouped) => ValidGroupByProjection (a /\ b) grouped

else instance ValidGroupByProjection q grouped


-- | GROUP BY statement
groupBy :: forall f s q sql grouped fields.
      ToGroupBy q s fields =>
      GroupByFields f fields grouped =>
      ValidGroupByProjection s grouped =>
      ToRest q (GroupBy f E) sql =>
      f -> q -> sql
groupBy f q = toRest q $ GroupBy f E



----------------------------AS----------------------------

newtype As (alias :: Symbol) rest = As rest

-- | Acceptable alias targets
class ToAs (q :: Type) (alias :: Symbol) | q -> alias

instance ToAs Int alias

instance ToAs (Table name fields) alias

instance ToAs (Proxy name) alias

instance ToAs (Path table name) alias

instance ToAs (Aggregate inp fields out) alias

instance ToAs (Select s p (From f fields rest)) alias


-- | AS statement
as :: forall q alias sql. ToAs q alias => ToRest q (As alias E) sql => Proxy alias -> q -> sql
as _ q = toRest q $ As E



---------------------------ORDER BY------------------------------------------

data OrderBy f rest = OrderBy f rest

data Sort (name :: Symbol) = Asc | Desc

--refactor: besides being repetitive, this doesnt allow table.column in order by
class ToOrderBy (f :: Type) (q :: Type)

instance (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields E))

instance (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields (GroupBy fd E)))

instance (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields (Where cd E)))

instance (Union projection fields all, ToOrderByFields f all) => ToOrderBy f (Select s projection (From fr fields (Where cd (GroupBy fd E))))


class ToOrderByFields (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToOrderByFields (Proxy name) fields

instance Cons name t e fields  => ToOrderByFields (Sort name) fields

instance (ToOrderByFields a fields, ToOrderByFields b fields) => ToOrderByFields (a /\ b) fields


-- | ASC
asc :: forall name. Proxy name -> Sort name
asc _ = Asc

-- | DESC
desc :: forall name. Proxy name -> Sort name
desc _ = Desc

-- | ORDER BY statement
orderBy :: forall f q sql. ToOrderBy f q => ToRest q (OrderBy f E) sql => f -> q -> sql
orderBy f q = toRest q $ OrderBy f E



------------------------LIMIT---------------------------

data Limit rest = Limit Int rest


class ToLimit (q :: Type)

instance ToLimit (Select s projection (From fr fields (OrderBy f E)))

instance ToLimit (Select s projection (From fr fields (GroupBy fg (OrderBy f E))))

instance ToLimit (Select s projection (From fr fields (Where cd (GroupBy fg (OrderBy f E)))))


-- | LIMIT statement
-- |
-- | Note: LIMIT must always follow after ORDER BY
limit :: forall q sql. ToLimit q => ToRest q (Limit E) sql => Int -> q -> sql
limit n q = toRest q $ Limit n E



------------------------COALESCE---------------------------

--a (special) function, but we have to define it here
-- data Coalesce q projection = Coalesce q

-- class ToCoalesce t projection | t -> projection

-- instance ToProjection (Select s p (From f fields rest)) fields projection => ToCoalesce (Select s p (From f fields rest)) projection

-- instance => Cons alias Int () projection => ToCoalesce (As alias Int) projection

-- instance (ToCoalesce a projection, ToCoalesce b projection) => ToCoalesce (Tuple a b) projection

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


class InsertList (fields :: Row Type) (fieldNames :: Type) (inserted :: Row Type) | fieldNames -> fields inserted

instance (InvalidField t, Cons name t e fields, Cons name t () single) => InsertList fields (Proxy name) single

instance (
      InsertList fields f head,
      InsertList fields rest tail,
      Union head tail all
) => InsertList fields  (f /\ rest) all


class RequiredFields (fieldList :: RowList Type) (required :: Row Type) | fieldList -> required

instance RequiredFields RL.Nil ()

instance RequiredFields rest required => RequiredFields (RL.Cons n (Auto t) rest) required

else instance RequiredFields rest required => RequiredFields (RL.Cons n (Default t) rest) required

else instance RequiredFields rest required => RequiredFields (RL.Cons n (Maybe t) rest) required

else instance (RequiredFields rest tail, Cons name t () head, Lacks name tail, Union head tail required) => RequiredFields (RL.Cons name t rest) required


class InsertValues (fields :: Row Type) (fieldNames :: Type) (t :: Type) | fieldNames -> fields t

instance (UnwrapDefinition t u, Cons name t e fields, ToValue u) => InsertValues fields (Proxy name) u

else instance (InsertValues fields name value, InsertValues fields some more) => InsertValues fields (name /\ some) (value /\ more)


insert :: Insert E
insert = Insert E

into :: forall tableName fields fieldNames fieldList required e inserted.
      RowToList fields fieldList =>
      RequiredFields fieldList required =>
      InsertList fields fieldNames inserted =>
      Union required e inserted =>
      Table tableName fields -> fieldNames -> Insert E -> Insert (Into tableName fields fieldNames E)
into _ fieldNames _ = Insert (Into fieldNames E)

values :: forall tableName fields fieldNames fieldValues. InsertValues fields fieldNames fieldValues => fieldValues -> Insert (Into tableName fields fieldNames E) -> Insert (Into tableName fields fieldNames (Values fieldValues E))
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

instance (
      InvalidField t,
      UnwrapDefinition t u,
      ToValue u,
      Cons name t e fields
) => ToUpdatePairs fields (Proxy name /\ u)

else instance (
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

newtype Returning f = Returning f


class ToReturning (f :: Type) (q :: Type) | q -> f

instance ToReturningFields f fields => ToReturning f (Insert (Into tn fields fn (Values fv E)))


class ToReturningFields (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToReturningFields (Proxy name) fields

instance (ToReturningFields a fields, ToReturningFields b fields) => ToReturningFields (a /\ b) fields


returning :: forall f q sql. ToReturning f q => ToRest q (Returning f) sql => f -> q -> sql
returning f q = toRest q $ Returning f



------------------------Projection machinery---------------------------

-- | Computes SELECT projection as a `Row Type`
class ToProjection :: forall k. Type -> Row Type -> k -> Row Type -> Constraint
class ToProjection s fields alias projection | s -> fields projection

-- | Columns
instance (
      UnwrapDefinition t u,
      Cons name t e fields,
      Cons name u () projection
) => ToProjection (Proxy name) fields alias projection

-- | Inner join path columns
else instance (
      AppendPath alias name fullPath,
      Cons fullPath t e fields,
      Cons fullPath t () projection
) => ToProjection (Path alias name) fields Inner projection

-- | Outer join path columns
else instance (
      AppendPath alias name fullPath,
      Cons fullPath t e fields,
      JoinedToMaybe t v,
      Cons fullPath v () projection
) => ToProjection (Path alias name) fields Outer projection

-- | Path column from current scope
else instance (
      UnwrapDefinition t u,
      Cons name t e fields,
      AppendPath alias name fullPath,
      Cons fullPath u () projection
) => ToProjection (Path alias name) fields alias projection

-- | Path column from outer scope
else instance (AppendPath table name fullPath, Cons fullPath (Path table name) () projection) => ToProjection (Path table name) fields alias projection

-- | Aliased literal
else instance Cons alias Int () projection => ToProjection (As alias Int) fields a projection

-- | Aliased aggregation
else instance Cons alias t () projection => ToProjection (As alias (Aggregate inp fields t)) fields a projection

-- | Aliased column
else instance (
      UnwrapDefinition t u,
      Cons name t e fields,
      Cons alias u () projection
) => ToProjection (As alias (Proxy name)) fields a projection

-- | Aliased join path column
else instance (
      AppendPath table name fullPath,
      Cons fullPath t e fields,
      Cons alias t () projection
) => ToProjection (As alias (Path table name)) fields Side projection

-- | Aliased path column from current scope
else instance (
      UnwrapDefinition t u,
      Cons name t e fields,
      Cons alias u () projection
) => ToProjection (As alias (Path table name)) fields table projection

-- | Aliased path column from outer scope
else instance Cons alias (Path table name) () projection => ToProjection (As alias (Path table name)) fields a projection

-- | All columns from source
else instance (RowToList fields list, UnwrapAll list projection) => ToProjection Star fields alias projection

-- | Column list
else instance (
      ToProjection s fields alias some,
      ToProjection t fields alias more,
      Union some more projection
) => ToProjection (s /\ t) fields alias projection

-- | Subquery as column
else instance (
      IsTableAliased f table,
      ToProjection s fields table projection,
      RowToList projection list,
      ToSingleColumn list name t,
      IsNamedSubQuery rest name alias, -- if the subquery ends in as
      Cons alias t () single
) => ToProjection (Select s p (From f fields rest)) fd a single


else instance Fail (Text "Cannot recognize projection") => ToProjection x f a p


--not required but makes for clearer type errors
class ToSingleColumn (fields :: RowList Type) (name :: Symbol) (t :: Type) | fields -> name t

instance ToSingleColumn (RL.Cons name (Maybe t) RL.Nil) name (Maybe t)

else instance ToSingleColumn (RL.Cons name t RL.Nil) name (Maybe t)


-- | Query projections should not repeat column names
class UniqueColumnNames (some :: Row Type) (more :: Row Type)

instance UniqueColumnNames fields fields


-- | Joined tables should not repeat table aliases
class UniqueAliases (some :: Row Type) (more :: Row Type)

instance UniqueAliases fields fields


class IsTableAliased (f :: Type) (alias :: Symbol) | f -> alias

instance IsTableAliased (As alias (Table name fields)) alias

else instance IsNamedSubQuery rest Empty alias => IsTableAliased (Select s p (From f fd rest)) alias

else instance IsTableAliased f Empty


-- | Find this query's alias, or fail at compile time if query is not aliased
class QueryMustBeAliased (q :: Type) (alias :: Symbol) | q -> alias

instance QueryMustBeAliased rest alias => QueryMustBeAliased (Where cd rest) alias

instance QueryMustBeAliased rest alias => QueryMustBeAliased (GroupBy f rest) alias

instance QueryMustBeAliased rest alias => QueryMustBeAliased (OrderBy f rest) alias

instance QueryMustBeAliased rest alias => QueryMustBeAliased (Limit rest) alias

instance Fail (Text "Expected query to end in AS statement") => QueryMustBeAliased E alias

instance QueryMustBeAliased (As alias E) alias


class IsNamedSubQuery (q :: Type) (name :: Symbol) (alias :: Symbol) | q -> name alias

instance IsNamedSubQuery rest name alias => IsNamedSubQuery (Where cd rest) name alias

instance IsNamedSubQuery rest name alias => IsNamedSubQuery (GroupBy f rest) name alias

instance IsNamedSubQuery rest name alias => IsNamedSubQuery (OrderBy f rest) name alias

instance IsNamedSubQuery rest name alias => IsNamedSubQuery (Limit rest) name alias

instance IsNamedSubQuery E name name

instance IsNamedSubQuery (As alias E) name alias


class UnwrapAll (list :: RowList Type) (projection :: Row Type) | list -> projection

instance UnwrapAll RL.Nil ()

instance (
      UnwrapDefinition t u,
      Cons name u () head,
      UnwrapAll rest tail,
      Union head tail projection
) => UnwrapAll (RL.Cons name t rest) projection


class AliasedFields (list :: RowList Type) (alias :: Symbol) (fields :: Row Type) | list alias -> fields

instance AliasedFields RL.Nil alias ()

instance (
      ToPath alias path,
      Append path name fullPath,
      Cons fullPath t () head,
      AliasedFields rest alias tail,
      Union head tail fields
) => AliasedFields (RL.Cons name t rest) alias fields

--refactor: this class is only needed for ToQuery (Select s projection (From f fields rest)) final, which might be able to do it in another way, and thus remove this
class ToPath (alias :: Symbol) (path :: Symbol) | alias -> path

instance ToPath Empty Empty

else instance Append alias Dot path => ToPath alias path


class JoinedToMaybe (t :: Type) (v :: Type) | t -> v

instance JoinedToMaybe (Joined (f (Maybe t))) (Maybe t)

else instance JoinedToMaybe (Joined (Maybe t)) (Maybe t)

else instance UnwrapDefinition t u => JoinedToMaybe (Joined t) (Maybe u)

else instance JoinedToMaybe t t


class AppendPath (alias :: Symbol) (name :: Symbol) (fullPath :: Symbol) | alias name -> fullPath

instance (Append alias Dot path, Append path name fullPath) => AppendPath alias name fullPath



---------------------------Rest machinery------------------------------------------

--this trick does the actual replacement of E for the next statement
-- we could use a alternative encoding for queries (e.g. tuples),
-- but I think this one is clearer (for the end user) when looking at the (final) types
class ToRest a b c | a -> b, a b -> c where
      toRest :: a -> b -> c

instance ToRest rest b c => ToRest (Select s p rest) b (Select s p c) where
      toRest (Select s rest) b = Select s $ toRest rest b

else instance ToRest rest b c => ToRest (From f fd rest) b (From f fd c) where
      toRest (From f rest) b = From f $ toRest rest b

else instance ToRest rest b c => ToRest (Where cd rest) b (Where cd c) where
      toRest (Where f rest) b = Where f $ toRest rest b

else instance ToRest rest b c => ToRest (GroupBy f rest) b (GroupBy f c) where
      toRest (GroupBy f rest) b = GroupBy f $ toRest rest b

else instance ToRest rest b c => ToRest (OrderBy f rest) b (OrderBy f c) where
      toRest (OrderBy f rest) b = OrderBy f $ toRest rest b

else instance ToRest rest b c => ToRest (Limit rest) b (Limit c) where
      toRest (Limit n rest) b = Limit n $ toRest rest b

else instance ToRest rest b c => ToRest (Update n f rest) b (Update n f c) where
      toRest (Update rest) b = Update $ toRest rest b

else instance ToRest rest b c => ToRest (Insert rest) b (Insert c) where
      toRest (Insert rest) b = Insert $ toRest rest b

else instance ToRest rest b c => ToRest (Into n f fd rest) b (Into n f fd c) where
      toRest (Into f rest) b = Into f $ toRest rest b

else instance ToRest rest b c => ToRest (Values v rest) b (Values v c) where
      toRest (Values v rest) b = Values v $ toRest rest b

else instance ToRest rest b c => ToRest (Set p rest) b (Set p c) where
      toRest (Set p rest) b = Set p $ toRest rest b

else instance ToRest rest b c => ToRest (Delete rest) b (Delete c) where
      toRest (Delete rest) b = Delete $ toRest rest b

else instance ToRest (As alias E) b (As alias b) where
      toRest (As _) b = As b

else instance ToRest E b b where
      toRest _ b = b

else instance ToRest b a c => ToRest a b c where
      toRest a b = toRest b a


data E = E