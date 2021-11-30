-- | This module defines the entire SQL eDSL, mostly because it'd be a pain to split it
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Syntax
      ( Join(..)
      , Inclusion(..)
      , Side
      , Inner
      , Outer
      , SymbolList
      , On(..)
      , Union(..)
      , As(..)
      , Delete(..)
      , From(..)
      , Insert(..)
      , OrderBy(..)
      , Into(..)
      , D
      , GroupBy(..)
      , Limit(..)
      , Plan(..)
      , Distinct(..)
      , Prepare(..)
      , Select(..)
      , Returning(..)
      , Set(..)
      , Update(..)
      , Create(..)
      , Values(..)
      , Offset(..)
      , Where(..)
      , Sort(..)
      , class SortFieldsSource
      , class IncludeColumn
      , class UnwrapAll
      , class Resume
      , class StarProjection
      , class SymbolListSingleton
      , class SourceAlias
      , class UpdatedFields
      , class ToPath
      , class QueryMustBeAliased
      , class UniqueSources
      , class OuterScopeAlias
      , class OnCondition
      , class IsDefault
      , class UnwrapDefault
      , class QueryOptionallyAliased
      , class ToJoin
      , class QualifiedColumn
      , class ExcludeIdentity
      , class OnComparision
      , class ConstraintsToRowList
      , class IncludeMandatoryField
      , class IncludesRequiredFields
      , class ValidGroupByProjection
      , class GroupByFields
      , class ToGroupBy
      , class ToOuterFields
      , class ToUnion
      , class RequiredColumns
      , class ToAs
      , class ToFrom
      , class GroupBySource
      , class IncludedFields
      , class InsertValues
      , class ToPrepare
      , class ToProjection
      , class ToSelect
      , class ToSingleColumn
      , class ToSubExpression
      , class IncludeAllColumns
      , class SourceFields
      , class UpdatedPairs
      , class ToReturning
      , class ToReturningFields
      , class UniqueAliases
      , class QualifiedFields
      , class ToWhere
      , class JoinedToMaybe
      , class CompatibleProjection
      , class UniqueColumnNames
      , class ToOrderBy
      , class SortFields
      , class ToLimit
      , class ToOffset
      , join
      , leftJoin
      , resume
      , exists
      , on
      , union
      , groupBy
      , unionAll
      , orderBy
      , distinct
      , offset
      , as
      , delete
      , asc
      , desc
      , table
      , from
      , insert
      , limit
      , into
      , prepare
      , select
      , set
      , update
      , values
      , returning
      , wher
      , create
      ) where

import Prelude
import Prim hiding (Constraint)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Condition (class ToCondition, class ValidComparision, Exists(..), Op(..), OuterScope)
import Droplet.Language.Internal.Definition (class AppendPath, class ToValue, class UnwrapNullable, Constraint, Default, Dot, E(..), Empty, Identity, Joined, Path, Star, Table(..))
import Droplet.Language.Internal.Function (class ToStringAgg, Aggregate, PgFunction)
import Prim.Boolean (False, True)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy)
import Type.RowList (class ListToRow, class RowListAppend, class RowListNub)

----------------------PREPARE----------------------------

data Prepare q = Prepare q Plan

-- | Name of this prepared statement
newtype Plan = Plan String

-- | Only complete statements are accepted by PREPARE
class ToPrepare (q ∷ Type)

instance ToPrepare (Select s p (From f fields rest))

instance ToPrepare (Insert (Into name fields inserted names (Values v rest)))

instance ToPrepare (Update name fields constraints (Set v rest))

instance ToPrepare (Delete (From f fields rest))

-- | https://www.postgresql.org/docs/current/sql-prepare.html
-- |
-- | PREPARE statements can be employed to reuse execution plans, and thus optimize performance
-- |
-- | Note: droplet always creates server-side parameters for literal values in queries. In the case of PREPARE, however, literals will be be parsed as statement parameters
prepare ∷ ∀ q. ToPrepare q ⇒ Plan → q → Prepare q
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
      integer | function | SELECT output_name

FROM
      table_name | SELECT AS

JOIN
     ON { field | parameter } OPERATOR { field | parameter } | [ { and | or } ] | [...]

WHERE
      { field | parameter } OPERATOR { field | parameter } | [ { and | or } ] | [...]

OPERATOR
      = | <> | not | in | exists

ORDER BY
      field { ASC | DESC } | function | [, ...]

LIMIT
      number

OFFSET
      number

-}

-- | SELECT representation. `projection` refers to the final output of this statement
data Select s (projection ∷ Row Type) rest = Select s rest

-- | Acceptable column type for SELECT statements
class ToSelect (s ∷ Type)

instance ToSelect (Proxy name)

else instance ToSelect (Path table name)

else instance ToSelect (As alias Int)

else instance ToSelect (As alias (Proxy name))

else instance ToSelect (As alias (Path table name))

else instance ToSelect (As alias (Aggregate inp rest fields out))

else instance ToSelect (As alias (PgFunction inp arg fields out))

else instance (ToSelect r, ToSelect t) ⇒ ToSelect (r /\ t)

else instance ToSelect Star

else instance ToSelect (Distinct s)

else instance ToSubExpression q ⇒ ToSelect q

-- | Only single columns can be projected by subqueries
-- |
-- | Note: column subqueries may not return a value, thus their projection will be `Maybe` unless the original column type is already `Maybe`
class ToSubExpression (s ∷ Type)

instance ToSubExpression (Select (Proxy name) projection rest)

instance ToSubExpression (Select (Path table name) projection rest)

instance ToSubExpression (Select (As alias Int) projection rest)

instance ToSubExpression (Select (As alias (Proxy name)) projection rest)

instance ToSubExpression (Select (As alias (Path table name)) projection rest)

instance ToSubExpression (Select (As alias (PgFunction inp arg fields out)) projection rest)

instance ToSubExpression (Select (As alias (Aggregate inp r fields out)) projection rest)

instance Fail (Text "Subquery must return a single column") ⇒ ToSubExpression (Select (a /\ b) projection rest)

-- | SELECT can project literals, columns and subqueries with the following considerations:
-- |
-- | - Multiple columns are represented by tuples. `Data.Tuple.Nested./\` is convenient for this
-- | - Literal values (e.g., numbers) must be aliased (with AS)
-- | - Columns names in projections must be unique, or referenced by different table aliases (e.g., u.name, t.name)
-- | - Subqueries must return a single column
select ∷ ∀ s projection. ToSelect s ⇒ s → Select s projection E
select s = Select s E

-------------------------------DISTINCT----------------------------

newtype Distinct s = Distinct s

distinct ∷ ∀ s. ToSelect s ⇒ s → Distinct s
distinct = Distinct

-------------------------------FROM----------------------------

--as it is, From does not try to carry all source fields (for example qualified and unqualified columns from table as alias)
-- reason being, it makes projection code a lot harder for joins and select *
-- this can change if a simpler design is found
data From f (fields ∷ Row Type) rest = From f rest

-- | Acceptable sources for FROM statements
class ToFrom (f ∷ Type) (q ∷ Type) (fields ∷ Row Type) | q f → fields

-- | (DELETE) FROM table
instance ToFrom (Table name fields constraints) (Delete E) fields

-- | FROM ... JOIN ...
else instance
      ( ToProjection s fields aliases selected
      , Nub selected unique
      , UniqueColumnNames selected unique
      ) ⇒
      ToFrom (Join k fields l r aliases (On c rest)) (Select s unique E) fields

-- | Anything `SourceFields` can compute
else instance
      ( SourceFields f fields aliases
      , ToProjection s fields aliases selected
      , Nub selected unique
      , UniqueColumnNames selected unique
      ) ⇒
      ToFrom f (Select s unique E) fields

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
from ∷ ∀ f q fields sql. ToFrom f q fields ⇒ Resume q (From f fields E) sql ⇒ f → q → sql
from f q = resume q $ From f E

-------------------------------JOIN----------------------------

-- | Kind for OUTER and INNER joins
data Side

foreign import data Inner ∷ Side
foreign import data Outer ∷ Side

data Join (k ∷ Side) (fields ∷ Row Type) q r (aliases ∷ SymbolList) rest = Join q r rest

data On c rest = On c rest

-- | Given a source `q`, compute its joined (non and qualified) fields
class ToJoin (q ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList) | q → fields aliases

-- | JOIN ... ON
instance
      ( ToJoin l left las
      , ToJoin r right ras
      , Union left right all
      , Nub all fields
      , RowListAppend las ras aliases
      ) ⇒
      ToJoin (Join Inner fd l r a (On c rest)) fields aliases

-- | outer JOIN ... ON
else instance
      ( ToJoin l left las
      , ToJoin r right ras
      , RowToList right list
      , ToOuterFields list out
      , Union left out all
      , Nub all fields
      , RowListAppend las ras aliases
      ) ⇒
      ToJoin (Join Outer fd l r a (On c rest)) fields aliases

else instance SourceFields q fields aliases ⇒ ToJoin q fields aliases

-- | OUTER JOINs make one side nullable, as a corresponding record may not be found
-- |
-- | For ease of use, this class marks the nullable side fields with `Joined`, later on `ToProjection` will flatten it to `Maybe`
class ToOuterFields (list ∷ RowList Type) (fields ∷ Row Type) | list → fields

instance ToOuterFields Nil ()

-- | Avoid nesting `Joined`s
instance
      ( Cons name (Joined t) () head
      , ToOuterFields rest tail
      , Union head tail all
      ) ⇒
      ToOuterFields (Cons name (Joined t) rest) all

else instance
      ( Cons name (Joined t) () head
      , ToOuterFields rest tail
      , Union head tail all
      ) ⇒
      ToOuterFields (Cons name t rest) all

-- | INNER JOIN statement
-- |
-- | JOIN sources are the same as FROM
join ∷
      ∀ r l aliases unique las ras right rf lf left all source fields.
      ToJoin l left las ⇒
      ToJoin r right ras ⇒
      UniqueSources left right ⇒
      RowListAppend las ras aliases ⇒
      RowListNub aliases unique ⇒
      UniqueAliases aliases unique ⇒
      Union right left all ⇒
      Nub all source ⇒
      Union left lf source ⇒
      Union right rf source ⇒
      Union lf rf fields ⇒
      l →
      r →
      Join Inner fields l r aliases E
join l r = Join l r E

-- | LEFT OUTER JOIN statement
-- |
-- | JOIN sources are the same as FROM
leftJoin ∷
      ∀ r l las ras list out aliases unique rf lf right left all fields source.
      ToJoin l left las ⇒
      ToJoin r right ras ⇒
      UniqueSources left right ⇒
      RowListAppend las ras aliases ⇒
      RowListNub aliases unique ⇒
      UniqueAliases aliases unique ⇒
      Union left right all ⇒
      Nub all source ⇒
      Union left lf source ⇒
      Union right rf source ⇒
      RowToList lf list ⇒
      ToOuterFields list out ⇒
      Union rf out fields ⇒
      l →
      r →
      Join Outer fields l r aliases E
leftJoin l r = Join l r E

--would be nice to have a unified (but not messy) way to reuse tocondition here
-- | Comparison logic for ON statements
class OnCondition (c ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList)

instance (OnCondition (Op a b) fields aliases, OnCondition (Op c d) fields aliases) ⇒ OnCondition (Op (Op a b) (Op c d)) fields aliases

else instance
      ( OnComparision a fields aliases t
      , OnComparision b fields aliases u
      , ValidComparision t u
      ) ⇒
      OnCondition (Op a b) fields aliases

class OnComparision (a ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList) (t ∷ Type) | a → t

instance
      ( Cons name t d fields
      , UnwrapNullable t u
      ) ⇒
      OnComparision (Proxy name) fields aliases u

else instance
      ( SymbolListSingleton alias single
      , RowListAppend single aliases all
      , RowListNub all unique
      , OuterScopeAlias all unique y
      , AppendPath alias name fullPath
      , QualifiedColumn y fullPath fields t
      , UnwrapNullable t u
      ) ⇒
      OnComparision (Path alias name) fields aliases u

else instance OnComparision (Path alias name) fields aliases OuterScope

else instance ToValue t ⇒ OnComparision t fields aliases t

-- | JOIN ... ON statement
on ∷ ∀ k l r c fields aliases. OnCondition c fields aliases ⇒ c → Join k fields l r aliases E → Join k fields l r aliases (On c E)
on c (Join q r _) = Join q r $ On c E

-------------------------------WHERE----------------------------

data Where c rest = Where c rest

-- | WHERE can only follow FROM, UPDATE and DELETE
class ToWhere (c ∷ Type) (q ∷ Type)

instance (SourceAlias f alias, ToCondition c fields alias) ⇒ ToWhere c (Select s projection (From f fields E))

instance ToCondition c fields Empty ⇒ ToWhere c (Update name fields constraints (Set v E))

instance ToCondition c fields Empty ⇒ ToWhere c (Delete (From f fields E))

-- | WHERE statement
wher ∷ ∀ c q sql. ToWhere c q ⇒ Resume q (Where c E) sql ⇒ c → q → sql
wher c q = resume q $ Where c E

----------------------------EXISTS----------------------------

exists ∷ ∀ s projection f fields rest. Select s projection (From f fields rest) → Op Exists (Select s projection (From f fields rest))
exists q = Op Nothing Exists q

----------------------------GROUP BY----------------------------

data GroupBy f rest = GroupBy f rest

-- | GROUP BY can only follow FROM or WHERE
class ToGroupBy (q ∷ Type) (s ∷ Type) (fields ∷ Row Type) | q → s fields

instance GroupBySource f fields ⇒ ToGroupBy (Select s p (From f fd E)) s fields

instance GroupBySource f fields ⇒ ToGroupBy (Select s p (From f fd (Where cond E))) s fields

class GroupBySource (f ∷ Type) (fields ∷ Row Type) | f → fields

--refactor: could be the same as tojoin if joins accepted non aliased tables
instance GroupBySource (Table name fields constraints) fields

instance
      ( RowToList fields list
      , QualifiedFields list alias aliased
      , Union aliased fields all
      ) ⇒
      GroupBySource (As alias (Table name fields constraints)) all

instance GroupBySource (Join k fields q r a rest) fields

instance
      ( QueryMustBeAliased rest alias
      , RowToList projection list
      , QualifiedFields list alias aliased
      , Union aliased projection all
      ) ⇒
      GroupBySource (Select s projection (From f fd rest)) all

class GroupByFields (f ∷ Type) (fields ∷ Row Type) (grouped ∷ Row Type) | f → fields grouped

instance (Cons name t e fields, Cons name t () grouped) ⇒ GroupByFields (Proxy name) fields grouped

instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e fields
      , Cons fullPath t () g
      , Cons name t g grouped
      ) ⇒
      GroupByFields (Path alias name) fields grouped

instance
      ( GroupByFields a fields some
      , GroupByFields b fields more
      , Union some more grouped
      ) ⇒
      GroupByFields (a /\ b) fields grouped

-- | Asserts that a SELECT ... GROUP BY projection contains only grouped columns or aggregate functions
class ValidGroupByProjection (s ∷ Type) (grouped ∷ Row Type) | s → grouped

instance Cons name t e grouped ⇒ ValidGroupByProjection (Proxy name) grouped

else instance Cons name t e grouped ⇒ ValidGroupByProjection (As alias (Proxy name)) grouped

else instance (ValidGroupByProjection a grouped, ValidGroupByProjection b grouped) ⇒ ValidGroupByProjection (a /\ b) grouped

else instance ValidGroupByProjection s grouped ⇒ ValidGroupByProjection (Distinct s) grouped

else instance ValidGroupByProjection q grouped

-- | GROUP BY statement
groupBy ∷
      ∀ f s q sql grouped fields.
      ToGroupBy q s fields ⇒
      GroupByFields f fields grouped ⇒
      ValidGroupByProjection s grouped ⇒
      Resume q (GroupBy f E) sql ⇒
      f →
      q →
      sql
groupBy f q = resume q $ GroupBy f E

----------------------------AS----------------------------

newtype As (alias ∷ Symbol) rest = As rest

-- | Acceptable alias targets
class ToAs (q ∷ Type) (alias ∷ Symbol) | q → alias

instance ToAs Int alias

instance ToAs (Table name fields constraints) alias

instance ToAs (Proxy name) alias

instance ToAs (Path table name) alias

instance ToAs (Aggregate inp rest fields out) alias

instance ToAs (PgFunction inp args fields out) alias

instance ToAs (Select s p (From f fields rest)) alias

-- | AS statement
as ∷ ∀ q alias sql. ToAs q alias ⇒ Resume q (As alias E) sql ⇒ Proxy alias → q → sql
as _ q = resume q $ As E

---------------------------ORDER BY------------------------------------------

data OrderBy f rest = OrderBy f rest

data Sort (f ∷ Type) = Asc | Desc

-- | ORDER BY must be last statement
class ToOrderBy (f ∷ Type) (q ∷ Type)

instance (SortFieldsSource s projection f fields available, SortFields st available) ⇒ ToOrderBy st (Select s projection (From f fields E))

instance (SortFieldsSource s projection f fields available, SortFields st available) ⇒ ToOrderBy st (Select s projection (From f fields (GroupBy g E)))

instance (SortFieldsSource s projection f fields available, SortFields st available) ⇒ ToOrderBy st (Select s projection (From f fields (Where cd E)))

instance (SortFieldsSource s projection f fields available, SortFields st available) ⇒ ToOrderBy st (Select s projection (From f fields (Where cd (GroupBy g E))))

-- for aggregate/window functions
instance ToOrderBy (Proxy name) String

instance ToOrderBy (Path alias name) String

--this error might not be clear for the user
-- | Fields available for sorting this query
-- |
-- | N.B: SELECT DISTINCT queries can only be sorted by fields in the projection
class SortFieldsSource (s ∷ Type) (projection ∷ Row Type) (f ∷ Type) (fields ∷ Row Type) (available ∷ Row Type) | s → available

instance SortFieldsSource (Distinct s) projection f fields projection

else instance
      ( SourceAlias f alias
      , RowToList fields list
      , QualifiedFields list alias qual
      , Union projection fields pf
      , Union qual pf all
      ) ⇒
      SortFieldsSource s projection f fields all

class SortFields (f ∷ Type) (fields ∷ Row Type) | f → fields

instance Cons name t e fields ⇒ SortFields (Proxy name) fields

--not allowing out of scope qualified fields yet
instance (AppendPath alias name fullPath, Cons fullPath t e fields) ⇒ SortFields (Path alias name) fields

instance Cons name t e fields ⇒ SortFields (Sort (Proxy name)) fields

instance (AppendPath alias name fullPath, Cons fullPath t e fields) ⇒ SortFields (Sort (Path alias name)) fields

instance Fail (Text "Cannot sort by void function") ⇒ SortFields (PgFunction input args fields Unit) fields

else instance SortFields (PgFunction input args fields output) fields

instance (SortFields a fields, SortFields b fields) ⇒ SortFields (a /\ b) fields

-- | ASC
asc ∷ ∀ name. name → Sort name
asc _ = Asc

-- | DESC
desc ∷ ∀ name. name → Sort name
desc _ = Desc

-- | ORDER BY statement
orderBy ∷ ∀ f q sql. ToOrderBy f q ⇒ Resume q (OrderBy f E) sql ⇒ f → q → sql
orderBy f q = resume q $ OrderBy f E

instance (Cons name t e fields, Cons fd g h fields) ⇒ ToStringAgg (Proxy name) (OrderBy (Proxy fd) String) fields

instance Cons name t e fields ⇒ ToStringAgg (Path table fd) (OrderBy (Proxy name) String) fields

instance Cons name t e fields ⇒ ToStringAgg (Proxy name) (OrderBy (Path alias fd) String) fields

instance ToStringAgg (Path table name) (OrderBy (Path alias fd) String) fields

------------------------LIMIT---------------------------

data Limit rest = Limit Int rest

class ToLimit (q ∷ Type)

instance ToLimit (Select s projection (From fr fields (OrderBy f E)))

instance ToLimit (Select s projection (From fr fields (OrderBy f (Offset E))))

instance ToLimit (Select s projection (From fr fields (GroupBy fg (OrderBy f E))))

instance ToLimit (Select s projection (From fr fields (GroupBy fg (OrderBy f (Offset E)))))

instance ToLimit (Select s projection (From fr fields (Where cd (OrderBy f E))))

instance ToLimit (Select s projection (From fr fields (Where cd (OrderBy f (Offset E)))))

instance ToLimit (Select s projection (From fr fields (Where cd (GroupBy fg (OrderBy f E)))))

instance ToLimit (Select s projection (From fr fields (Where cd (GroupBy fg (OrderBy f (Offset E))))))

-- | LIMIT statement
-- |
-- | Note: LIMIT must always follow after ORDER BY
limit ∷ ∀ q sql. ToLimit q ⇒ Resume q (Limit E) sql ⇒ Int → q → sql
limit n q = resume q $ Limit n E

------------------------OFFSET---------------------------

data Offset rest = Offset Int rest

class ToOffset (q ∷ Type)

instance ToOffset (Select s projection (From fr fields (OrderBy f E)))

instance ToOffset (Select s projection (From fr fields (OrderBy f (Limit E))))

instance ToOffset (Select s projection (From fr fields (GroupBy fg (OrderBy f E))))

instance ToOffset (Select s projection (From fr fields (GroupBy fg (OrderBy f (Limit E)))))

instance ToOffset (Select s projection (From fr fields (Where cd (OrderBy f E))))

instance ToOffset (Select s projection (From fr fields (Where cd (OrderBy f (Limit E)))))

instance ToOffset (Select s projection (From fr fields (Where cd (GroupBy fg (OrderBy f E)))))

instance ToOffset (Select s projection (From fr fields (Where cd (GroupBy fg (OrderBy f (Limit E))))))

-- | OFFSET statement
-- |
-- | Note: OFFSET must always follow after LIMIT or ORDER BY
offset ∷ ∀ q sql. ToOffset q ⇒ Resume q (Offset E) sql ⇒ Int → q → sql
offset n q = resume q $ Offset n E

------------------------UNION---------------------------

data Union q r = Union Inclusion q r

data Inclusion = All | Unique

class ToUnion (q ∷ Type) (r ∷ Type)

instance
      ( RowToList some slist
      , RowToList more mlist
      , CompatibleProjection slist mlist
      ) ⇒
      ToUnion (Select s some (From f fd rt)) (Select t more (From g ge es))

instance ToUnion (Select s p (From f fd rt)) (Select t q (From g ge es)) ⇒ ToUnion (Union (Select s p (From f fd rt)) sel) (Select t q (From g ge es))

instance ToUnion (Select s p (From f fd rt)) (Select t q (From g ge es)) ⇒ ToUnion (Select s p (From f fd rt)) (Union (Select t q (From g ge es)) sel)

class CompatibleProjection (pro ∷ RowList Type) (jection ∷ RowList Type)

instance CompatibleProjection Nil Nil

else instance CompatibleProjection some more ⇒ CompatibleProjection (Cons name t some) (Cons n t more)

else instance Fail (Text "UNION column types and count must match") ⇒ CompatibleProjection a b

union ∷ ∀ q r. ToUnion q r ⇒ q → r → Union q r
union = Union Unique

unionAll ∷ ∀ q r. ToUnion q r ⇒ q → r → Union q r
unionAll = Union All

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
      [VALUES values | DEFAULT ]

-}

newtype Insert rest = Insert rest

data Into (name ∷ Symbol) (fields ∷ Row Type) (inserted ∷ Row Type) names rest = Into names rest

data Values fieldValues rest = Values fieldValues rest

-- | Trick to support default values
data D (t ∷ Type)

-- | Select identity (can't be inserted) and default (don't need to be inserted) constraints
class ConstraintsToRowList (source ∷ Type) (constraints ∷ RowList Type) | source → constraints

instance ConstraintsToRowList Unit Nil

instance
      ( ConstraintsToRowList (Constraint name f t) head
      , ConstraintsToRowList (Constraint name rest t) tail
      , RowListAppend head tail all
      ) ⇒
      ConstraintsToRowList (Constraint name (f /\ rest) t) all

else instance ConstraintsToRowList (Constraint name field Identity) (Cons field Identity Nil)

else instance ConstraintsToRowList (Constraint name field Default) (Cons field Default Nil)

else instance ConstraintsToRowList (Constraint name field t) Nil

instance
      ( ConstraintsToRowList c head
      , ConstraintsToRowList rest tail
      , RowListAppend head tail all
      ) ⇒
      ConstraintsToRowList (c /\ rest) all

-- | Valid insert/update fields
class IncludedFields (names ∷ Type) (fields ∷ Row Type) (constraints ∷ RowList Type) (included ∷ Row Type) | names → included

instance
      ( Cons name t e fields
      , ExcludeIdentity name t constraints single
      ) ⇒
      IncludedFields (Proxy name) fields constraints single

instance
      ( IncludedFields n fields constraints head
      , IncludedFields rest fields constraints tail
      , Union head tail all
      ) ⇒
      IncludedFields (n /\ rest) fields constraints all

-- | Exclude field if identity
class ExcludeIdentity (name ∷ Symbol) (t ∷ Type) (constraints ∷ RowList Type) (single ∷ Row Type) | name → single

instance Cons name t () single ⇒ ExcludeIdentity name t Nil single

else instance Cons name (D t) () single ⇒ ExcludeIdentity name t (Cons name Default rest) single

--this clause is never kicking in
else instance
      ( Append "Identity column " name start
      , Append start " cannot be inserted or updated" finish
      , Fail (Text finish)
      ) ⇒
      ExcludeIdentity name t (Cons name Identity rest) ()

else instance ExcludeIdentity name t rest single ⇒ ExcludeIdentity name t (Cons other u rest) single

-- | Fields that must be inserted
class RequiredColumns (fields ∷ RowList Type) (constraints ∷ RowList Type) (required ∷ Row Type) | fields → required

instance RequiredColumns Nil constraints ()

else instance
      ( IncludeMandatoryField name t constraints head
      , RequiredColumns rest constraints tail
      , Union head tail required
      ) ⇒
      RequiredColumns (Cons name t rest) constraints required

-- | Include a field if it is not in the (identity/default) constraints list
class IncludeMandatoryField (name ∷ Symbol) (t ∷ Type) (constraints ∷ RowList Type) (single ∷ Row Type) | name → single

instance IncludeMandatoryField name (Maybe t) constraints ()

else instance Cons name t () single ⇒ IncludeMandatoryField name t Nil single

else instance IncludeMandatoryField name t (Cons name Identity rest) ()

else instance IncludeMandatoryField name t (Cons name Default rest) ()

else instance IncludeMandatoryField name t rest single ⇒ IncludeMandatoryField name t (Cons other u rest) single

-- | Inserted values must match insert list
class InsertValues (fields ∷ Row Type) (names ∷ Type) (t ∷ Type)

-- | Multiple values, single column
instance InsertValues fields (Proxy name) u ⇒ InsertValues fields (Proxy name) (Array u)

-- | DEFAULT
else instance (Cons name t e fields, IsDefault t name) ⇒ InsertValues fields (Proxy name) Default

-- | Values
else instance
      ( Cons name u e fields
      , UnwrapDefault u t
      , ToValue t
      ) ⇒
      InsertValues fields (Proxy name) t

-- | Column list
else instance (InsertValues fields name value, InsertValues fields some more) ⇒ InsertValues fields (name /\ some) (value /\ more)

-- | Multiple values, many columns
else instance (InsertValues fields (name /\ some) (value /\ more)) ⇒ InsertValues fields (name /\ some) (Array (value /\ more))

-- | Clearer error message in case of misplaced default value
class IsDefault (t ∷ Type) (name ∷ Symbol) | t → name

instance IsDefault (D t) name

else instance
      ( Append "Column " name start
      , Append start " does not have a DEFAULT constraint" message
      , Fail (Text message)
      ) ⇒
      IsDefault t name

class UnwrapDefault (t ∷ Type) (u ∷ Type) | t → u

instance UnwrapDefault (D t) t

else instance UnwrapDefault t t

--suboptimal error messages!
-- | Inserted fields contain required fields
class IncludesRequiredFields (required ∷ Row Type) (inserted ∷ Row Type)

instance Union required e inserted ⇒ IncludesRequiredFields required inserted

insert ∷ Insert E
insert = Insert E

into ∷
      ∀ tableName fields names constraints fieldList required constraintList inserted.
      ConstraintsToRowList constraints constraintList ⇒
      RowToList fields fieldList ⇒
      IncludedFields names fields constraintList inserted ⇒
      RequiredColumns fieldList constraintList required ⇒
      IncludesRequiredFields required inserted ⇒
      Table tableName fields constraints →
      names →
      Insert E →
      Insert (Into tableName fields inserted names E)
into _ names _ = Insert (Into names E)

values ∷ ∀ tableName fields names inserted values. InsertValues inserted names values ⇒ values → Insert (Into tableName fields inserted names E) → Insert (Into tableName fields inserted names (Values values E))
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

newtype Update (name ∷ Symbol) (fields ∷ Row Type) (constraints ∷ Type) rest = Update rest

data Set pairs rest = Set pairs rest

-- | Select all fields being updated
class UpdatedFields (pairs ∷ Type) (names ∷ Type) | pairs → names

instance UpdatedFields (Op name t) name

instance
      ( UpdatedFields head some
      , UpdatedFields tail more
      ) ⇒
      UpdatedFields (head /\ tail) (some /\ more)

-- | Updated fields must be valid
class UpdatedPairs (fields ∷ Row Type) (pairs ∷ Type)

-- | Default
instance (Cons name t e fields, IsDefault t name) ⇒ UpdatedPairs fields (Op (Proxy name) Default)

-- | Value
else instance
      ( Cons name u e fields
      , UnwrapDefault u t
      , ToValue t
      ) ⇒
      UpdatedPairs fields (Op (Proxy name) t)

instance
      ( UpdatedPairs fields head
      , UpdatedPairs fields tail
      ) ⇒
      UpdatedPairs fields (head /\ tail)

update ∷ ∀ tableName fields constraints. Table tableName fields constraints → Update tableName fields constraints E
update _ = Update E

set ∷
      ∀ tableName names fields constraints updated constraintList pairs.
      ConstraintsToRowList constraints constraintList ⇒
      UpdatedFields pairs names ⇒
      IncludedFields names fields constraintList updated ⇒
      UpdatedPairs updated pairs ⇒
      pairs →
      Update tableName fields constraints E →
      Update tableName fields constraints (Set pairs E)
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

delete ∷ Delete E
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

class ToReturning (f ∷ Type) (q ∷ Type) | q → f

instance ToReturningFields f fields ⇒ ToReturning f (Insert (Into tn fields ins fn (Values fv E)))

class ToReturningFields (f ∷ Type) (fields ∷ Row Type) | f → fields

instance Cons name t e fields ⇒ ToReturningFields (Proxy name) fields

instance (ToReturningFields a fields, ToReturningFields b fields) ⇒ ToReturningFields (a /\ b) fields

returning ∷ ∀ f q sql. ToReturning f q ⇒ Resume q (Returning f) sql ⇒ f → q → sql
returning f q = resume q $ Returning f

------------------------Projection machinery---------------------------

-- | A `RowList` of `Symbol`s
type SymbolList = RowList Symbol

-- | Computes SELECT projection as a `Row Type`
class ToProjection (s ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList) (projection ∷ Row Type) | s → fields projection

-- | Columns
instance
      ( Cons name t e fields
      , JoinedToMaybe t v
      , Cons name v () projection
      ) ⇒
      ToProjection (Proxy name) fields aliases projection

-- | Qualified column
-- |
-- | Outer scope columns are validated once the full query is known (i.e., before running it)
else instance
      ( SymbolListSingleton alias single
      , RowListAppend single aliases all
      , RowListNub all unique
      , OuterScopeAlias all unique y
      , AppendPath alias name fullPath
      , QualifiedColumn y fullPath fields t
      , Cons fullPath t () projection
      ) ⇒
      ToProjection (Path alias name) fields aliases projection

-- | Aliased literal
else instance Cons alias Int () projection ⇒ ToProjection (As alias Int) fields aliases projection

-- | Aliased aggregation
else instance Cons alias t () projection ⇒ ToProjection (As alias (Aggregate inp rest fields t)) fields aliases projection

-- | Aliased function
else instance Cons alias t () projection ⇒ ToProjection (As alias (PgFunction inp args fields t)) fields aliases projection

-- | Aliased column
else instance
      ( Cons name t e fields
      , JoinedToMaybe t v
      , Cons alias v () projection
      ) ⇒
      ToProjection (As alias (Proxy name)) fields aliases projection

-- | Aliased qualified column
-- |
-- | Outer scope columns are validated once the full query is known (i.e., before running it)
else instance
      ( SymbolListSingleton table single
      , RowListAppend single aliases all
      , RowListNub all unique
      , OuterScopeAlias all unique y
      , AppendPath table name fullPath
      , QualifiedColumn y fullPath fields t
      , Cons alias t () projection
      ) ⇒
      ToProjection (As alias (Path table name)) fields aliases projection

-- | All columns from source
else instance (RowToList fields list, StarProjection list fields aliases projection) ⇒ ToProjection Star fields aliases projection

-- | Column list
else instance
      ( ToProjection s fields aliases some
      , ToProjection t fields aliases more
      , Union some more projection
      ) ⇒
      ToProjection (s /\ t) fields aliases projection

-- | Subquery as column
else instance
      ( --does source has an alias? we need it to figure out which qualified fields can be validated here
        SourceAlias f table
      , SymbolListSingleton table single
      , ToProjection s fields single pro
      , RowToList pro list
      --is column already Maybe or needs to be made into Maybe? subquery can only return a single column
      , ToSingleColumn list name t
      --is query aliased? if so we have to use the alias instead of the column name
      , QueryOptionallyAliased rest name alias
      , Cons alias t () projection
      ) ⇒
      ToProjection (Select s p (From f fields rest)) fd aliases projection

-- | DISTINCT
else instance ToProjection s fields aliases projection ⇒ ToProjection (Distinct s) fields aliases projection

-- | Any valid instance should be recognizable
else instance Fail (Text "Cannot recognize projection") ⇒ ToProjection x f a p

-- | Is this column present in the current field source?
class OuterScopeAlias (all ∷ SymbolList) (unique ∷ SymbolList) (y ∷ Boolean) | all unique → y

instance OuterScopeAlias Nil Nil True

else instance OuterScopeAlias (Cons alias alias some) Nil False

else instance OuterScopeAlias Nil (Cons alias alias some) False

else instance (OuterScopeAlias some more y) ⇒ OuterScopeAlias (Cons alias alias some) (Cons alias alias more) y

else instance OuterScopeAlias (Cons alias alias some) (Cons otherAlias otherAlias more) False

-- | The type of a qualified column
class QualifiedColumn (unscoped ∷ Boolean) (fullPath ∷ Symbol) (fields ∷ Row Type) (t ∷ Type) | unscoped → fullPath fields t

instance QualifiedColumn True fullPath fields OuterScope

instance
      ( Cons fullPath t d fields
      , UnwrapNullable t u
      ) ⇒
      QualifiedColumn False fullPath fields u

--not required but makes for clearer type errors
class ToSingleColumn (fields ∷ RowList Type) (name ∷ Symbol) (t ∷ Type) | fields → name t

instance ToSingleColumn (Cons name (Maybe t) Nil) name (Maybe t)

else instance ToSingleColumn (Cons name t Nil) name (Maybe t)

-- | Query projections should not repeat column names
class UniqueColumnNames (some ∷ Row Type) (more ∷ Row Type)

instance UniqueColumnNames fields fields

-- | Joined tables should not be the same
class UniqueSources (some ∷ Row Type) (more ∷ Row Type)

instance (Fail (Text "Cannot JOIN source to itself")) ⇒ UniqueSources fields fields

else instance UniqueSources some more

--needs to be improved for clarity
-- | Joined tables should not repeat table aliases
class UniqueAliases (some ∷ SymbolList) (more ∷ SymbolList)

instance UniqueAliases aliases aliases

-- | Table/subquery alias or `Empty`
class SourceAlias (f ∷ Type) (alias ∷ Symbol) | f → alias

instance SourceAlias (As alias (Table name fields constraints)) alias

else instance QueryOptionallyAliased rest Empty alias ⇒ SourceAlias (Select s p (From f fd rest)) alias

else instance SourceAlias f Empty

-- | Find this query's alias, or fail at compile time if query is not aliased
class QueryMustBeAliased (q ∷ Type) (alias ∷ Symbol) | q → alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (Where cd rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (GroupBy f rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (OrderBy f rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (Limit rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (Offset rest) alias

instance Fail (Text "Expected query to end in AS statement") ⇒ QueryMustBeAliased E alias

instance QueryMustBeAliased (As alias E) alias

-- | If this query is in the form of (SELECT ...) AS alias, return `alias`, otherwise keep `name`
class QueryOptionallyAliased (q ∷ Type) (name ∷ Symbol) (alias ∷ Symbol) | q → name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (Where cd rest) name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (GroupBy f rest) name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (OrderBy f rest) name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (Limit rest) name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (Offset rest) name alias

instance QueryOptionallyAliased E name name

instance QueryOptionallyAliased (As alias E) name alias

-- | SELECT * FROM should:
-- |
-- | - Display column unqualified if it appear both as qualified and unqualified
-- | - Display column qualified if is projected with `Path`
class StarProjection (list ∷ RowList Type) (fields ∷ Row Type) (aliases ∷ SymbolList) (projection ∷ Row Type) | list → projection

--very slow and kind of clunky, but an easy way to un-qualify and select distinct columns
-- the trick is that the union of the original fields with all columns qualified by all aliases will yield only unqualified columns
instance
      ( IncludeAllColumns list aliases included
      , ListToRow included extended
      , Union fields extended all
      , Nub all nubbed
      , Union extended unqualified nubbed
      , RowToList unqualified ulist
      , UnwrapAll ulist projection
      ) ⇒
      StarProjection list fields aliases projection

-- | Recursively call `IncludeColumn` on the given list
class IncludeAllColumns (list ∷ RowList Type) (aliases ∷ SymbolList) (all ∷ RowList Type) | list → all

instance IncludeAllColumns Nil aliases Nil

instance
      ( IncludeColumn name t aliases included
      , IncludeAllColumns rest aliases more
      , RowListAppend included more all
      ) ⇒
      IncludeAllColumns (Cons name t rest) aliases all

-- | Build a `RowList` of the given name and type qualified with each alias
class IncludeColumn (name ∷ Symbol) (t ∷ Type) (aliases ∷ SymbolList) (included ∷ RowList Type) | name → included

instance IncludeColumn name t Nil Nil

instance (AppendPath alias name fullPath, IncludeColumn name t rest included) ⇒ IncludeColumn name t (Cons alias alias rest) (Cons fullPath t included)

-- | Recursively remove source field wrappers
class UnwrapAll (list ∷ RowList Type) (projection ∷ Row Type) | list → projection

instance UnwrapAll Nil ()

instance
      ( Cons name t () head
      , UnwrapAll rest tail
      , Union head tail projection
      ) ⇒
      UnwrapAll (Cons name t rest) projection

-- | Computes all source fields with their alias
class QualifiedFields (list ∷ RowList Type) (alias ∷ Symbol) (fields ∷ Row Type) | list alias → fields

instance QualifiedFields Nil alias ()

instance
      ( ToPath alias path
      , Append path name fullPath
      , Cons fullPath t () head
      , QualifiedFields rest alias tail
      , Union head tail fields
      ) ⇒
      QualifiedFields (Cons name t rest) alias fields

-- | Optionally add source field alias
class ToPath (alias ∷ Symbol) (path ∷ Symbol) | alias → path

instance ToPath Empty Empty

else instance Append alias Dot path ⇒ ToPath alias path

-- | `Joined` fields appear as `Maybe` in projections
class JoinedToMaybe (t ∷ Type) (v ∷ Type) | t → v

instance JoinedToMaybe (Joined (Maybe t)) (Maybe t)

else instance JoinedToMaybe (Joined t) (Maybe t)

else instance JoinedToMaybe t t

-- | Creates a `SymbolList` single with a single entry
class SymbolListSingleton (alias ∷ Symbol) (list ∷ SymbolList) | alias → list

instance SymbolListSingleton alias (Cons alias alias Nil)

-- | Given a source `f`, compute its (non and qualified) fields
class SourceFields (f ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList) | f → fields aliases

-- | Tables
instance SourceFields (Table name fields constraints) fields Nil

-- | Aliased tables
instance
      ( RowToList source list
      , QualifiedFields list alias aliased
      , Union aliased source fields
      , SymbolListSingleton alias single
      ) ⇒
      SourceFields (As alias (Table name source constraints)) fields single

-- | Aliased subqueries
instance
      ( QueryMustBeAliased rest alias
      , RowToList projection list
      , QualifiedFields list alias aliased
      , Union projection aliased fields
      , SymbolListSingleton alias single
      ) ⇒
      SourceFields (Select s projection (From f fd rest)) fields single

---------------------------Resume machinery------------------------------------------

-- | Most SQL statement constructors accept a `rest` type parameter that refers to next statements
-- |
-- | Such parameter is initially filled with `E`, meaning that the query ends there
-- |
-- | This type class replaces the (nested) final `E` for the next statement
class Resume a b c | a → b, a b → c where
      resume ∷ a → b → c

instance Resume rest b c ⇒ Resume (Select s p rest) b (Select s p c) where
      resume (Select s rest) b = Select s $ resume rest b

else instance Resume rest b c ⇒ Resume (From f fd rest) b (From f fd c) where
      resume (From f rest) b = From f $ resume rest b

else instance Resume rest b c ⇒ Resume (Where cd rest) b (Where cd c) where
      resume (Where f rest) b = Where f $ resume rest b

else instance Resume rest b c ⇒ Resume (GroupBy f rest) b (GroupBy f c) where
      resume (GroupBy f rest) b = GroupBy f $ resume rest b

else instance Resume rest b c ⇒ Resume (OrderBy f rest) b (OrderBy f c) where
      resume (OrderBy f rest) b = OrderBy f $ resume rest b

else instance Resume rest b c ⇒ Resume (Limit rest) b (Limit c) where
      resume (Limit n rest) b = Limit n $ resume rest b

else instance Resume rest b c ⇒ Resume (Offset rest) b (Offset c) where
      resume (Offset n rest) b = Offset n $ resume rest b

else instance Resume rest b c ⇒ Resume (Update n f constraints rest) b (Update n f constraints c) where
      resume (Update rest) b = Update $ resume rest b

else instance Resume rest b c ⇒ Resume (Insert rest) b (Insert c) where
      resume (Insert rest) b = Insert $ resume rest b

else instance Resume rest b c ⇒ Resume (Into n f ins fd rest) b (Into n f ins fd c) where
      resume (Into f rest) b = Into f $ resume rest b

else instance Resume rest b c ⇒ Resume (Values v rest) b (Values v c) where
      resume (Values v rest) b = Values v $ resume rest b

else instance Resume rest b c ⇒ Resume (Set p rest) b (Set p c) where
      resume (Set p rest) b = Set p $ resume rest b

else instance Resume rest b c ⇒ Resume (Delete rest) b (Delete c) where
      resume (Delete rest) b = Delete $ resume rest b

else instance Resume (As alias E) b (As alias b) where
      resume (As _) b = As b

else instance Resume E b b where
      resume _ b = b

else instance Resume b a c ⇒ Resume a b c where
      resume a b = resume b a

---------------------------CREATE------------------------------------------

newtype Create rest = Create rest

create ∷ Create E
create = Create E

---------------------------OR REPLACE------------------------------------------


---------------------------TABLE------------------------------------------

{-

full create table syntax supported by postgresql (https://www.postgresql.org/docs/current/sql-createtable.html)

CREATE [ [ GLOBAL | LOCAL ] { TEMPORARY | TEMP } | UNLOGGED ] TABLE [ IF NOT EXISTS ] table_name ( [
  { column_name data_type [ COMPRESSION compression_method ] [ COLLATE collation ] [ column_constraint [ ... ] ]
    | table_constraint
    | LIKE source_table [ like_option ... ] }
    [, ... ]
] )
[ INHERITS ( parent_table [, ... ] ) ]
[ PARTITION BY { RANGE | LIST | HASH } ( { column_name | ( expression ) } [ COLLATE collation ] [ opclass ] [, ... ] ) ]
[ USING method ]
[ WITH ( storage_parameter [= value] [, ... ] ) | WITHOUT OIDS ]
[ ON COMMIT { PRESERVE ROWS | DELETE ROWS | DROP } ]
[ TABLESPACE tablespace_name ]

CREATE [ [ GLOBAL | LOCAL ] { TEMPORARY | TEMP } | UNLOGGED ] TABLE [ IF NOT EXISTS ] table_name
    OF type_name [ (
  { column_name [ WITH OPTIONS ] [ column_constraint [ ... ] ]
    | table_constraint }
    [, ... ]
) ]
[ PARTITION BY { RANGE | LIST | HASH } ( { column_name | ( expression ) } [ COLLATE collation ] [ opclass ] [, ... ] ) ]
[ USING method ]
[ WITH ( storage_parameter [= value] [, ... ] ) | WITHOUT OIDS ]
[ ON COMMIT { PRESERVE ROWS | DELETE ROWS | DROP } ]
[ TABLESPACE tablespace_name ]

CREATE [ [ GLOBAL | LOCAL ] { TEMPORARY | TEMP } | UNLOGGED ] TABLE [ IF NOT EXISTS ] table_name
    PARTITION OF parent_table [ (
  { column_name [ WITH OPTIONS ] [ column_constraint [ ... ] ]
    | table_constraint }
    [, ... ]
) ] { FOR VALUES partition_bound_spec | DEFAULT }
[ PARTITION BY { RANGE | LIST | HASH } ( { column_name | ( expression ) } [ COLLATE collation ] [ opclass ] [, ... ] ) ]
[ USING method ]
[ WITH ( storage_parameter [= value] [, ... ] ) | WITHOUT OIDS ]
[ ON COMMIT { PRESERVE ROWS | DELETE ROWS | DROP } ]
[ TABLESPACE tablespace_name ]

where column_constraint is:

[ CONSTRAINT constraint_name ]
{ NOT NULL |
  NULL |
  CHECK ( expression ) [ NO INHERIT ] |
  DEFAULT default_expr |
  GENERATED ALWAYS AS ( generation_expr ) STORED |
  GENERATED { ALWAYS | BY DEFAULT } AS IDENTITY [ ( sequence_options ) ] |
  UNIQUE index_parameters |
  PRIMARY KEY index_parameters |
  REFERENCES reftable [ ( refcolumn ) ] [ MATCH FULL | MATCH PARTIAL | MATCH SIMPLE ]
    [ ON DELETE referential_action ] [ ON UPDATE referential_action ] }
[ DEFERRABLE | NOT DEFERRABLE ] [ INITIALLY DEFERRED | INITIALLY IMMEDIATE ]

and table_constraint is:

[ CONSTRAINT constraint_name ]
{ CHECK ( expression ) [ NO INHERIT ] |
  UNIQUE ( column_name [, ... ] ) index_parameters |
  PRIMARY KEY ( column_name [, ... ] ) index_parameters |
  EXCLUDE [ USING index_method ] ( exclude_element WITH operator [, ... ] ) index_parameters [ WHERE ( predicate ) ] |
  FOREIGN KEY ( column_name [, ... ] ) REFERENCES reftable [ ( refcolumn [, ... ] ) ]
    [ MATCH FULL | MATCH PARTIAL | MATCH SIMPLE ] [ ON DELETE referential_action ] [ ON UPDATE referential_action ] }
[ DEFERRABLE | NOT DEFERRABLE ] [ INITIALLY DEFERRED | INITIALLY IMMEDIATE ]

and like_option is:

{ INCLUDING | EXCLUDING } { COMMENTS | COMPRESSION | CONSTRAINTS | DEFAULTS | GENERATED | IDENTITY | INDEXES | STATISTICS | STORAGE | ALL }

and partition_bound_spec is:

IN ( partition_bound_expr [, ...] ) |
FROM ( { partition_bound_expr | MINVALUE | MAXVALUE } [, ...] )
  TO ( { partition_bound_expr | MINVALUE | MAXVALUE } [, ...] ) |
WITH ( MODULUS numeric_literal, REMAINDER numeric_literal )

index_parameters in UNIQUE, PRIMARY KEY, and EXCLUDE constraints are:

[ INCLUDE ( column_name [, ... ] ) ]
[ WITH ( storage_parameter [= value] [, ... ] ) ]
[ USING INDEX TABLESPACE tablespace_name ]

exclude_element in an EXCLUDE constraint is:

{ column_name | ( expression ) } [ opclass ] [ ASC | DESC ] [ NULLS { FIRST | LAST } ]

-}

{-

full create table syntax supported by droplet

CREATE TABLE table_definition

where table_definition is the Table type
-}

--NEEDS SECURITY CHECKS
table ∷ ∀ name fields constraints. Create E → Table name fields constraints → Create (Table name fields constraints)
table _ _ = Create Table



---------------------------ALTER------------------------------------------

---------------------------TABLE------------------------------------------

{-

full alter table syntax supported by postgresql (https://www.postgresql.org/docs/current/sql-altertable.html)

ALTER TABLE [ IF EXISTS ] [ ONLY ] name [ * ]
    action [, ... ]
ALTER TABLE [ IF EXISTS ] [ ONLY ] name [ * ]
    RENAME [ COLUMN ] column_name TO new_column_name
ALTER TABLE [ IF EXISTS ] [ ONLY ] name [ * ]
    RENAME CONSTRAINT constraint_name TO new_constraint_name
ALTER TABLE [ IF EXISTS ] name
    RENAME TO new_name
ALTER TABLE [ IF EXISTS ] name
    SET SCHEMA new_schema
ALTER TABLE ALL IN TABLESPACE name [ OWNED BY role_name [, ... ] ]
    SET TABLESPACE new_tablespace [ NOWAIT ]
ALTER TABLE [ IF EXISTS ] name
    ATTACH PARTITION partition_name { FOR VALUES partition_bound_spec | DEFAULT }
ALTER TABLE [ IF EXISTS ] name
    DETACH PARTITION partition_name [ CONCURRENTLY | FINALIZE ]

where action is one of:

    ADD [ COLUMN ] [ IF NOT EXISTS ] column_name data_type [ COLLATE collation ] [ column_constraint [ ... ] ]
    DROP [ COLUMN ] [ IF EXISTS ] column_name [ RESTRICT | CASCADE ]
    ALTER [ COLUMN ] column_name [ SET DATA ] TYPE data_type [ COLLATE collation ] [ USING expression ]
    ALTER [ COLUMN ] column_name SET DEFAULT expression
    ALTER [ COLUMN ] column_name DROP DEFAULT
    ALTER [ COLUMN ] column_name { SET | DROP } NOT NULL
    ALTER [ COLUMN ] column_name DROP EXPRESSION [ IF EXISTS ]
    ALTER [ COLUMN ] column_name ADD GENERATED { ALWAYS | BY DEFAULT } AS IDENTITY [ ( sequence_options ) ]
    ALTER [ COLUMN ] column_name { SET GENERATED { ALWAYS | BY DEFAULT } | SET sequence_option | RESTART [ [ WITH ] restart ] } [...]
    ALTER [ COLUMN ] column_name DROP IDENTITY [ IF EXISTS ]
    ALTER [ COLUMN ] column_name SET STATISTICS integer
    ALTER [ COLUMN ] column_name SET ( attribute_option = value [, ... ] )
    ALTER [ COLUMN ] column_name RESET ( attribute_option [, ... ] )
    ALTER [ COLUMN ] column_name SET STORAGE { PLAIN | EXTERNAL | EXTENDED | MAIN }
    ALTER [ COLUMN ] column_name SET COMPRESSION compression_method
    ADD table_constraint [ NOT VALID ]
    ADD table_constraint_using_index
    ALTER CONSTRAINT constraint_name [ DEFERRABLE | NOT DEFERRABLE ] [ INITIALLY DEFERRED | INITIALLY IMMEDIATE ]
    VALIDATE CONSTRAINT constraint_name
    DROP CONSTRAINT [ IF EXISTS ]  constraint_name [ RESTRICT | CASCADE ]
    DISABLE TRIGGER [ trigger_name | ALL | USER ]
    ENABLE TRIGGER [ trigger_name | ALL | USER ]
    ENABLE REPLICA TRIGGER trigger_name
    ENABLE ALWAYS TRIGGER trigger_name
    DISABLE RULE rewrite_rule_name
    ENABLE RULE rewrite_rule_name
    ENABLE REPLICA RULE rewrite_rule_name
    ENABLE ALWAYS RULE rewrite_rule_name
    DISABLE ROW LEVEL SECURITY
    ENABLE ROW LEVEL SECURITY
    FORCE ROW LEVEL SECURITY
    NO FORCE ROW LEVEL SECURITY
    CLUSTER ON index_name
    SET WITHOUT CLUSTER
    SET WITHOUT OIDS
    SET TABLESPACE new_tablespace
    SET { LOGGED | UNLOGGED }
    SET ( storage_parameter [= value] [, ... ] )
    RESET ( storage_parameter [, ... ] )
    INHERIT parent_table
    NO INHERIT parent_table
    OF type_name
    NOT OF
    OWNER TO { new_owner | CURRENT_ROLE | CURRENT_USER | SESSION_USER }
    REPLICA IDENTITY { DEFAULT | USING INDEX index_name | FULL | NOTHING }

and partition_bound_spec is:

IN ( partition_bound_expr [, ...] ) |
FROM ( { partition_bound_expr | MINVALUE | MAXVALUE } [, ...] )
  TO ( { partition_bound_expr | MINVALUE | MAXVALUE } [, ...] ) |
WITH ( MODULUS numeric_literal, REMAINDER numeric_literal )

and column_constraint is:

[ CONSTRAINT constraint_name ]
{ NOT NULL |
  NULL |
  CHECK ( expression ) [ NO INHERIT ] |
  DEFAULT default_expr |
  GENERATED ALWAYS AS ( generation_expr ) STORED |
  GENERATED { ALWAYS | BY DEFAULT } AS IDENTITY [ ( sequence_options ) ] |
  UNIQUE index_parameters |
  PRIMARY KEY index_parameters |
  REFERENCES reftable [ ( refcolumn ) ] [ MATCH FULL | MATCH PARTIAL | MATCH SIMPLE ]
    [ ON DELETE referential_action ] [ ON UPDATE referential_action ] }
[ DEFERRABLE | NOT DEFERRABLE ] [ INITIALLY DEFERRED | INITIALLY IMMEDIATE ]

and table_constraint is:

[ CONSTRAINT constraint_name ]
{ CHECK ( expression ) [ NO INHERIT ] |
  UNIQUE ( column_name [, ... ] ) index_parameters |
  PRIMARY KEY ( column_name [, ... ] ) index_parameters |
  EXCLUDE [ USING index_method ] ( exclude_element WITH operator [, ... ] ) index_parameters [ WHERE ( predicate ) ] |
  FOREIGN KEY ( column_name [, ... ] ) REFERENCES reftable [ ( refcolumn [, ... ] ) ]
    [ MATCH FULL | MATCH PARTIAL | MATCH SIMPLE ] [ ON DELETE referential_action ] [ ON UPDATE referential_action ] }
[ DEFERRABLE | NOT DEFERRABLE ] [ INITIALLY DEFERRED | INITIALLY IMMEDIATE ]

and table_constraint_using_index is:

    [ CONSTRAINT constraint_name ]
    { UNIQUE | PRIMARY KEY } USING INDEX index_name
    [ DEFERRABLE | NOT DEFERRABLE ] [ INITIALLY DEFERRED | INITIALLY IMMEDIATE ]

index_parameters in UNIQUE, PRIMARY KEY, and EXCLUDE constraints are:

[ INCLUDE ( column_name [, ... ] ) ]
[ WITH ( storage_parameter [= value] [, ... ] ) ]
[ USING INDEX TABLESPACE tablespace_name ]

exclude_element in an EXCLUDE constraint is:

{ column_name | ( expression ) } [ opclass ] [ ASC | DESC ] [ NULLS { FIRST | LAST } ]

-}