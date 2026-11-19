-- | This module defines the entire SQL eDSL, mostly because it'd be a pain to split it
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Syntax
      ( Join(..)
      , Inclusion(..)
      , Side
      , Inner
      , Create(..)
      , Outer
      , On(..)
      , Union(..)
      , As(..)
      , Delete(..)
      , From(..)
      , Insert(..)
      , OrderBy(..)
      , Limit(..)
      , GroupBy(..)
      , DefaultValues(..)
      , Into(..)
      , Plan(..)
      , Distinct(..)
      , Where(..)
      , Sort(..)
      , Prepare(..)
      , Select(..)
      , Returning(..)
      , T(..)
      , Set(..)
      , Drop(..)
      , Alter(..)
      , Add(..)
      , Update(..)
      , Values(..)
      , Offset(..)
      , SymbolList
      , class SelectQuery
      , class IsSubQuery
      , class ToOffset
      , class SingleTypeComposite
      , class SortColumnsSource
      , class ValidColumnNames
      , class IncludeColumn
      , class ColumnCannotBeSet
      , class MultipleInsert
      , class SingleInsert
      , class OnlyAggregations
      , class ConsistentArity
      , class LimitedResults
      , class ToAdd
      , class ColumnNames
      , class ColumnTypes
      , class UniqueColumnNames
      , class UniqueTableColumnNames
      , class IsRequiredColumn
      , class TableChecks
      , class ColumHasType
      , class IncludeConstraint
      , class ToOrderBy
      , class SortColumns
      , class ConstraintsToRowList
      , class CheckComposite
      , class ValidComposites
      , class ValidConstraints
      , class ToLimit
      , class UniqueConstraints
      , class IsRepeated
      , class ToTable
      , class MatchingForeignKey
      , class ValidNullableConstraints
      , class StarProjection
      , class IsValidNullableConstraint
      , class IsDefault
      , class SymbolListSingleton
      , class SourceAlias
      , class MissingRequiredColumns
      , class ToPath
      , class OuterScopeAlias
      , class OnCondition
      , class ToJoin
      , class QualifiedColumn
      , class OnComparision
      , class ValidGroupByProjection
      , class GroupedColumns
      , class ToGroupBy
      , class ToOuterColumns
      , class ToWhere
      , class CompatibleProjection
      , class ToUnion
      , class RequiredColumns
      , class ToAs
      , class ToFrom
      , class InsertList
      , class InsertValues
      , class ToPrepare
      , class ToProjection
      , class ToSelect
      , class ToSelectColumn
      , class ToSingleColumn
      , class ToSubSelectColumn
      , class IncludeAllColumns
      , class SourceColumns
      , class ToUpdatePairs
      , class ToReturning
      , class ReturningColumns
      , class UniqueAliases
      , class QualifiedColumns
      , join
      , create
      , table
      , leftJoin
      , exists
      , defaultValues
      , on
      , union
      , groupBy
      , alter
      , drop
      , unionAll
      , distinct
      , orderBy
      , offset
      , as
      , add
      , delete
      , asc
      , desc
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
      ) where

import Prelude
import Prim hiding (Constraint)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Condition (class ToCondition, class ValidComparision, Exists(..), Op(..), OuterScope)
import Droplet.Language.Internal.Definition (class AppendPath, class ColumnInScope, class ToType, class ToValue, class UnwrapDefinition, class UnwrapNullable, C, Column, Composite, Constraint, Default, Dot, Empty, ForeignKey, Identity, Path, PrimaryKey, Star, Table, Unique)
import Droplet.Language.Internal.Function (class ToArrayAgg, class ToStringAgg, Aggregate, PgFunction)
import Prim.Boolean (False, True)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Beside, Quote, QuoteLabel, Text)
import Type.Data.Boolean (class And, class If)
import Type.Proxy (Proxy)
import Type.RowList (class ListToRow, class RowListAppend, class RowListNub)

{-

Statements are represented as nested data types where the last clause written is the outermost type, e.g.

      select id # from users # wher (id .=. 1) # orderBy id # limit (Proxy ∷ _ 1)

has type

      Limit 1 (OrderBy (Proxy "id") (Where (Op (Proxy "id") Int) (From (Table "users" Users) Users (id ∷ Int) (Select (Proxy "id")))))

Each combinator checks the outermost type of the statement it receives (i.e., what clause it follows) and wraps it

Keywords that carry no information (INSERT, CREATE, ALTER, DROP, DELETE) are represented by empty data types

-}

----------------------PREPARE----------------------------

data Prepare q = Prepare q Plan

-- | Name of this prepared statement
newtype Plan = Plan String

-- | Only complete statements are accepted by PREPARE
class ToPrepare (q ∷ Type)

instance ToPrepare (Values v (Into name fields fieldNames))

else instance ToPrepare (Returning f (Values v (Into name fields fieldNames)))

else instance ToPrepare (Set v (Update name fields))

else instance ToPrepare (Where c (Set v (Update name fields)))

else instance ToPrepare (From f fields projection Delete)

else instance ToPrepare (Where c (From f fields projection Delete))

else instance SelectQuery q s f fields projection ⇒ ToPrepare q

-- | https://www.postgresql.org/docs/current/sql-prepare.html
-- |
-- | PREPARE statements can be employed to reuse execution plans, and thus optimize performance
-- |
-- | Note: droplet always creates server-side parameters for literal values in queries. In the case of PREPARE, however, literals will be parsed as statement parameters
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

-- | SELECT representation. The projection (final output) of a statement is computed by FROM
data Select s = Select s

-- | Acceptable SELECT projection list
class ToSelect (s ∷ Type)

instance ToSelectColumn s ⇒ ToSelect (Distinct s)

else instance ToSelectColumn s ⇒ ToSelect s

-- | Acceptable column types for SELECT statements
class ToSelectColumn (s ∷ Type)

instance ToSelectColumn (Proxy name)

else instance ToSelectColumn (Path table name)

else instance ToSelectColumn (As alias Int)

else instance ToSelectColumn (As alias Boolean)

else instance ToSelectColumn (As alias (Op one another))

else instance ToSelectColumn (As alias (Proxy name))

else instance ToSelectColumn (As alias (Path table name))

else instance ToSelectColumn (As alias (Aggregate inp rest fields out))

else instance ToSelectColumn (As alias (PgFunction inp arg fields out))

else instance (ToSelectColumn r, ToSelectColumn t) ⇒ ToSelectColumn (r /\ t)

else instance ToSelectColumn Star

else instance Fail (Text "DISTINCT must follow SELECT") ⇒ ToSelectColumn (Distinct s)

-- | Aliased subquery
else instance (SelectQuery q s f fields projection, ToSubSelectColumn s, LimitedResults q) ⇒ ToSelectColumn (As alias q)

-- | Subquery
else instance (SelectQuery q s f fields projection, ToSubSelectColumn s, LimitedResults q) ⇒ ToSelectColumn q

-- | Only single columns can be projected by subqueries
class ToSubSelectColumn (f ∷ Type)

instance ToSubSelectColumn (Proxy name)

instance ToSubSelectColumn (Path table name)

instance ToSubSelectColumn (As alias Int)

instance ToSubSelectColumn (As alias Boolean)

instance ToSubSelectColumn (As alias (Proxy name))

instance ToSubSelectColumn (As alias (Path table name))

instance ToSubSelectColumn (As alias (PgFunction inp arg fields out))

instance ToSubSelectColumn (As alias (Aggregate inp r fields out))

instance Fail (Text "Subqueries must return a single column") ⇒ ToSubSelectColumn (a /\ b)

-- | Subqueries must return a single result
class LimitedResults (q ∷ Type)

instance LimitedResults (Limit 1 q)

else instance LimitedResults q ⇒ LimitedResults (Offset q)

else instance Fail (Text "Subqueries must return zero or one rows. Are you missing ORDER BY ... LIMIT 1?") ⇒ LimitedResults q

-- | SELECT can project literals, columns and subqueries with the following considerations:
-- |
-- | - Multiple columns are represented by tuples. `Data.Tuple.Nested./\` is convenient for this
-- | - Literal values (e.g., numbers) must be aliased (with AS)
-- | - Columns names in projections must be unique, or referenced by different table aliases (e.g., u.name, t.name)
-- | - Subqueries must return a single column
select ∷ ∀ s. ToSelect s ⇒ s → Select s
select = Select

-- | Finds the SELECT ... FROM core of a statement, skipping over any trailing clauses (WHERE, GROUP BY, ORDER BY, LIMIT, OFFSET)
-- |
-- | `s` is the select list, `f` the FROM source, `fields` the source columns and `projection` the statement output
class SelectQuery (q ∷ Type) (s ∷ Type) (f ∷ Type) (fields ∷ Row Type) (projection ∷ Row Type) | q → s f fields projection

instance SelectQuery (From f fields projection (Select s)) s f fields projection

else instance SelectQuery q s f fields projection ⇒ SelectQuery (Where c q) s f fields projection

else instance SelectQuery q s f fields projection ⇒ SelectQuery (GroupBy g q) s f fields projection

else instance SelectQuery q s f fields projection ⇒ SelectQuery (OrderBy o q) s f fields projection

else instance SelectQuery q s f fields projection ⇒ SelectQuery (Limit n q) s f fields projection

else instance SelectQuery q s f fields projection ⇒ SelectQuery (Offset q) s f fields projection

else instance Fail (Text "Expected a SELECT ... FROM ... statement") ⇒ SelectQuery q s f fields projection

-- | Whether a projected expression is a subquery (which must end in LIMIT or OFFSET, and may be aliased)
class IsSubQuery (q ∷ Type) (is ∷ Boolean) | q → is

instance IsSubQuery (Limit n q) True

else instance IsSubQuery (Offset q) True

else instance IsSubQuery (As alias (Limit n q)) True

else instance IsSubQuery (As alias (Offset q)) True

else instance IsSubQuery q False

-------------------------------DISTINCT----------------------------

newtype Distinct s = Distinct s

distinct ∷ ∀ s. ToSelectColumn s ⇒ s → Distinct s
distinct = Distinct

-------------------------------FROM----------------------------

--as it is, From does not try to carry all source fields (for example qualified and unqualified columns from table as alias) because it makes projection code a lot harder for joins and select *
-- this can change if a simpler design is found
data From f (fields ∷ Row Type) (projection ∷ Row Type) q = From f q

-- | Acceptable sources for FROM statements
class ToFrom (f ∷ Type) (q ∷ Type) (fields ∷ Row Type) (projection ∷ Row Type) | q f → fields projection

-- | (DELETE) FROM table
instance (RowToList fields list, ColumnTypes list columns) ⇒ ToFrom (Table name fields) Delete columns ()

-- | FROM ... JOIN ...
else instance
      ( ToProjection s fields aliases selected
      , Nub selected unique
      , UniqueColumnNames selected unique
      ) ⇒
      ToFrom (On c (Join k fields l r aliases)) (Select s) fields unique

-- | Anything `SourceColumns` can compute
else instance
      ( SourceColumns f fields aliases
      , ToProjection s fields aliases selected
      , Nub selected unique
      , UniqueColumnNames selected unique
      ) ⇒
      ToFrom f (Select s) fields unique

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
from ∷ ∀ f q fields projection. ToFrom f q fields projection ⇒ f → q → From f fields projection q
from f q = From f q

-------------------------------JOIN----------------------------

-- | Kind for OUTER and INNER joins
data Side

foreign import data Inner ∷ Side
foreign import data Outer ∷ Side

data Join (k ∷ Side) (fields ∷ Row Type) q r (aliases ∷ SymbolList) = Join q r

data On c j = On c j

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
      ToJoin (On c (Join Inner fd l r a)) fields aliases

-- | outer JOIN ... ON
else instance
      ( ToJoin l left las
      , ToJoin r right ras
      , RowToList right list
      , ToOuterColumns list out
      , Union left out all
      , Nub all fields
      , RowListAppend las ras aliases
      ) ⇒
      ToJoin (On c (Join Outer fd l r a)) fields aliases

else instance SourceColumns q fields aliases ⇒ ToJoin q fields aliases

-- | OUTER JOINs make one side nullable, as a corresponding record may not be found
class ToOuterColumns (list ∷ RowList Type) (fields ∷ Row Type) | list → fields

instance ToOuterColumns Nil ()

-- | Already nullable columns are kept as they are
instance
      ( Cons name (Maybe t) () head
      , ToOuterColumns rest tail
      , Union head tail all
      ) ⇒
      ToOuterColumns (Cons name (Maybe t) rest) all

else instance
      ( Cons name (Maybe t) () head
      , ToOuterColumns rest tail
      , Union head tail all
      ) ⇒
      ToOuterColumns (Cons name t rest) all

-- | INNER JOIN statement
-- |
-- | JOIN sources are the same as FROM
join ∷
      ∀ r l aliases unique las ras right rf lf left all source fields.
      ToJoin l left las ⇒
      ToJoin r right ras ⇒
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
      Join Inner fields l r aliases
join l r = Join l r

-- | LEFT OUTER JOIN statement
-- |
-- | JOIN sources are the same as FROM
leftJoin ∷
      ∀ r l las ras list out aliases unique rf lf right left all fields source.
      ToJoin l left las ⇒
      ToJoin r right ras ⇒
      RowListAppend las ras aliases ⇒
      RowListNub aliases unique ⇒
      UniqueAliases aliases unique ⇒
      Union left right all ⇒
      Nub all source ⇒
      Union left lf source ⇒
      Union right rf source ⇒
      RowToList lf list ⇒
      ToOuterColumns list out ⇒
      Union rf out fields ⇒
      l →
      r →
      Join Outer fields l r aliases
leftJoin l r = Join l r

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

instance (RowToList fields list, ColumnInScope name list t, UnwrapNullable t v) ⇒ OnComparision (Proxy name) fields aliases v

else instance
      ( SymbolListSingleton alias single
      , RowListAppend single aliases all
      , RowListNub all unique
      , OuterScopeAlias all unique y
      , AppendPath alias name fullPath
      , QualifiedColumn y fullPath fields t
      ) ⇒
      OnComparision (Path alias name) fields aliases t

else instance OnComparision (Path alias name) fields aliases OuterScope

else instance ToValue t ⇒ OnComparision t fields aliases t

-- | JOIN ... ON statement
on ∷ ∀ k l r c fields aliases. OnCondition c fields aliases ⇒ c → Join k fields l r aliases → On c (Join k fields l r aliases)
on c j = On c j

-------------------------------WHERE----------------------------

data Where c q = Where c q

-- | WHERE can only follow FROM (SELECT and DELETE) and SET (UPDATE)
class ToWhere (c ∷ Type) (q ∷ Type)

instance (SourceAlias f alias, ToCondition c fields alias) ⇒ ToWhere c (From f fields projection q)

else instance (RowToList fields list, ColumnTypes list columns, ToCondition c columns Empty) ⇒ ToWhere c (Set v (Update name fields))

else instance Fail (Text "WHERE must follow FROM or SET") ⇒ ToWhere c q

-- | WHERE statement
wher ∷ ∀ c q. ToWhere c q ⇒ c → q → Where c q
wher c q = Where c q

----------------------------EXISTS----------------------------

exists ∷ ∀ q s f fields projection. SelectQuery q s f fields projection ⇒ q → Op Exists q
exists q = Op Nothing Exists q

----------------------------GROUP BY----------------------------

data GroupBy f q = GroupBy f q

-- | GROUP BY can only follow FROM or WHERE. Grouped columns are the FROM source columns
class ToGroupBy (q ∷ Type) (s ∷ Type) (columns ∷ Row Type) | q → s columns

instance ToGroupBy (From f columns projection (Select s)) s columns

else instance ToGroupBy (Where c (From f columns projection (Select s))) s columns

else instance Fail (Text "GROUP BY must follow FROM or WHERE") ⇒ ToGroupBy q s columns

-- |
class GroupedColumns (f ∷ Type) (columns ∷ Row Type) (grouped ∷ Row Type) | f → columns grouped

instance (Cons name t e columns, Cons name t () grouped) ⇒ GroupedColumns (Proxy name) columns grouped

instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e columns
      , Cons fullPath t () g
      , Cons name t g grouped
      ) ⇒
      GroupedColumns (Path alias name) columns grouped

instance
      ( GroupedColumns a columns some
      , GroupedColumns b columns more
      , Union some more grouped
      ) ⇒
      GroupedColumns (a /\ b) columns grouped

-- | Asserts that a SELECT ... GROUP BY projection contains only grouped columns or aggregate functions
class ValidGroupByProjection (s ∷ Type) (grouped ∷ Row Type) | s → grouped

instance Cons name t e grouped ⇒ ValidGroupByProjection (Proxy name) grouped

instance Cons name t e grouped ⇒ ValidGroupByProjection (As alias (Proxy name)) grouped

instance (AppendPath table column name, Cons name t e grouped) ⇒ ValidGroupByProjection (Path table column) grouped

instance Cons name t e grouped ⇒ ValidGroupByProjection (As alias (Aggregate (Proxy name) rest fields out)) grouped

instance (ValidGroupByProjection a grouped, ValidGroupByProjection b grouped) ⇒ ValidGroupByProjection (a /\ b) grouped

instance ValidGroupByProjection s grouped ⇒ ValidGroupByProjection (Distinct s) grouped

-- | Are all columns aggregated?
class OnlyAggregations (q ∷ Type) (is ∷ Boolean) | q → is

instance OnlyAggregations (As n (Aggregate i rest f o)) True

else instance
      ( OnlyAggregations a isa
      , OnlyAggregations b isb
      , And isa isb is
      ) ⇒
      OnlyAggregations (a /\ b) is

else instance OnlyAggregations s False

-- | GROUP BY statement
groupBy ∷
      ∀ f s q grouped columns.
      ToGroupBy q s columns ⇒
      GroupedColumns f columns grouped ⇒
      ValidGroupByProjection s grouped ⇒
      f →
      q →
      GroupBy f q
groupBy f q = GroupBy f q

----------------------------AS----------------------------

newtype As (alias ∷ Symbol) q = As q

-- | Acceptable alias targets
class ToAs (q ∷ Type)

instance ToAs Int

else instance ToAs Boolean

else instance ToAs (Table name fields)

else instance ToAs (Proxy name)

else instance ToAs (Path table name)

else instance ToAs (Op one another)

else instance ToAs (Aggregate inp rest fields out)

else instance ToAs (PgFunction inp args fields out)

else instance SelectQuery q s f fields projection ⇒ ToAs q

-- | AS statement
as ∷ ∀ @alias q. ToAs q ⇒ q → As alias q
as q = As q

---------------------------ORDER BY------------------------------------------

data OrderBy f q = OrderBy f q

data Sort (f ∷ Type) = Asc | Desc

-- | ORDER BY can only follow FROM, WHERE or GROUP BY
class ToOrderBy (f ∷ Type) (q ∷ Type)

instance (SortColumnsSource s projection f columns available, SortColumns st available) ⇒ ToOrderBy st (From f columns projection (Select s))

else instance ToOrderBy st (From f columns projection (Select s)) ⇒ ToOrderBy st (Where c (From f columns projection (Select s)))

--only grouped columns are allowed here
else instance
      ( SelectQuery q s f columns projection
      , GroupedColumns g columns grouped
      , Union projection grouped pg
      , Nub pg available
      , SortColumns st available
      ) ⇒
      ToOrderBy st (GroupBy g q)

-- for aggregate/window functions
else instance ToOrderBy (Proxy name) String

else instance ToOrderBy (Path alias name) String

else instance ToOrderBy (Proxy name) (Proxy otherName)

else instance ToOrderBy (Path alias name) (Path otherAlias otherName)

else instance Fail (Text "ORDER BY must follow FROM, WHERE or GROUP BY") ⇒ ToOrderBy f q

--must be typed here
instance
      ( Cons name t e fields
      , Cons otherName v r fields
      ) ⇒
      ToArrayAgg (OrderBy (Proxy name) (Proxy otherName)) fields v

instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e fields
      , AppendPath otherAlias otherName otherFullPath
      , Cons otherFullPath v r fields
      ) ⇒
      ToArrayAgg (OrderBy (Path alias name) (Path otherAlias otherName)) fields v

--this error might not be clear for the user
-- | Columns available for sorting this query
-- |
-- | N.B: SELECT DISTINCT queries can only be sorted by columns in the projection
class SortColumnsSource (s ∷ Type) (projection ∷ Row Type) (f ∷ Type) (columns ∷ Row Type) (available ∷ Row Type) | s → available

instance SortColumnsSource (Distinct s) projection f columns projection

else instance
      ( SourceAlias f alias
      , RowToList columns list
      , QualifiedColumns list alias qual
      , Union projection columns pf
      , Union qual pf all
      , OnlyAggregations s only
      , If only projection all available
      ) ⇒
      SortColumnsSource s projection f columns available

-- |
class SortColumns (f ∷ Type) (columns ∷ Row Type) | f → columns

instance Cons name t e columns ⇒ SortColumns (Proxy name) columns

--not allowing out of scope qualified columns yet
instance (AppendPath alias name fullPath, Cons fullPath t e columns) ⇒ SortColumns (Path alias name) columns

instance Cons name t e columns ⇒ SortColumns (Sort (Proxy name)) columns

instance (AppendPath alias name fullPath, Cons fullPath t e columns) ⇒ SortColumns (Sort (Path alias name)) columns

instance Fail (Text "Cannot sort by void function") ⇒ SortColumns (PgFunction input args columns Unit) columns

else instance SortColumns (PgFunction input args columns output) columns

instance (SortColumns a columns, SortColumns b columns) ⇒ SortColumns (a /\ b) columns

-- | ASC
asc ∷ ∀ name. name → Sort name
asc _ = Asc

-- | DESC
desc ∷ ∀ name. name → Sort name
desc _ = Desc

-- | ORDER BY statement
orderBy ∷ ∀ f q. ToOrderBy f q ⇒ f → q → OrderBy f q
orderBy f q = OrderBy f q

instance (Cons name t e columns, Cons fd g h columns) ⇒ ToStringAgg (Proxy name) (OrderBy (Proxy fd) String) columns

instance Cons name t e columns ⇒ ToStringAgg (Path table fd) (OrderBy (Proxy name) String) columns

instance Cons name t e columns ⇒ ToStringAgg (Proxy name) (OrderBy (Path alias fd) String) columns

instance ToStringAgg (Path table name) (OrderBy (Path alias fd) String) columns

------------------------LIMIT---------------------------

data Limit (n ∷ Int) q = Limit q

-- | LIMIT can only follow ORDER BY or OFFSET
class ToLimit (q ∷ Type)

instance SelectQuery q s f fields projection ⇒ ToLimit (OrderBy o q)

else instance SelectQuery q s f fields projection ⇒ ToLimit (Offset (OrderBy o q))

else instance Fail (Text "LIMIT must follow ORDER BY") ⇒ ToLimit q

-- | LIMIT statement
-- |
-- | Note: LIMIT must always follow after ORDER BY
-- | e.g. `orderBy id # limit @10`
limit ∷ ∀ @n q. ToLimit q ⇒ q → Limit n q
limit q = Limit q

------------------------OFFSET---------------------------

data Offset q = Offset Int q

-- | OFFSET can only follow ORDER BY or LIMIT
class ToOffset (q ∷ Type)

instance SelectQuery q s f fields projection ⇒ ToOffset (OrderBy o q)

else instance SelectQuery q s f fields projection ⇒ ToOffset (Limit n (OrderBy o q))

else instance Fail (Text "OFFSET must follow ORDER BY or LIMIT") ⇒ ToOffset q

-- | OFFSET statement
-- |
-- | Note: OFFSET must always follow after LIMIT or ORDER BY
offset ∷ ∀ q. ToOffset q ⇒ Int → q → Offset q
offset n q = Offset n q

------------------------UNION---------------------------

data Union q r = Union Inclusion q r

data Inclusion = All | Unique

class ToUnion (q ∷ Type) (r ∷ Type)

instance ToUnion q r ⇒ ToUnion (Union q u) r

else instance ToUnion q r ⇒ ToUnion q (Union r u)

else instance
      ( SelectQuery q s f fields some
      , SelectQuery r t g columns more
      , RowToList some slist
      , RowToList more mlist
      , CompatibleProjection slist mlist
      ) ⇒
      ToUnion q r

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

data Insert = Insert

insert ∷ Insert
insert = Insert

data Into (name ∷ Symbol) (fields ∷ Row Type) fieldNames = Into fieldNames

-- | Compute list of inserted fields
class InsertList (fields ∷ Row Type) (fieldNames ∷ Type) (inserted ∷ Row Type) | fieldNames → fields inserted

instance InsertList fields DefaultValues ()

instance
      ( ColumnCannotBeSet t
      , Cons name t e fields
      , Cons name t () single
      ) ⇒
      InsertList fields (Proxy name) single

instance
      ( InsertList fields f head
      , InsertList fields rest tail
      , Union head tail all
      ) ⇒
      InsertList fields (f /\ rest) all

-- | Compute list of required fields
class RequiredColumns (fieldList ∷ RowList Type) (required ∷ Row Type) | fieldList → required

instance RequiredColumns Nil ()

else instance
      ( IsRequiredColumn t is
      , Cons name t () h
      , If is h () head
      , RequiredColumns rest tail
      , Union head tail required
      ) ⇒
      RequiredColumns (Cons name t rest) required

class IsRequiredColumn (t ∷ Type) (required ∷ Boolean) | t → required

instance
      ( IsRequiredColumn t tp
      , IsRequiredColumn constraints cn
      , And tp cn required
      ) ⇒
      IsRequiredColumn (Column t constraints) required

else instance IsRequiredColumn t required ⇒ IsRequiredColumn (Constraint n t) required

else instance IsRequiredColumn (Maybe t) False

else instance
      ( IsRequiredColumn some s
      , IsRequiredColumn more m
      , And s m required
      ) ⇒
      IsRequiredColumn (some /\ more) required

else instance IsRequiredColumn Default False

else instance IsRequiredColumn Identity False

else instance IsRequiredColumn t True

-- | Fields that cannot be inserted or updated
class ColumnCannotBeSet (t ∷ Type)

instance ColumnCannotBeSet constraints ⇒ ColumnCannotBeSet (Column t constraints)

else instance ColumnCannotBeSet t ⇒ ColumnCannotBeSet (Constraint n t)

else instance Fail (Text "Identity columns cannot be inserted or updated") ⇒ ColumnCannotBeSet Identity

else instance (ColumnCannotBeSet some, ColumnCannotBeSet more) ⇒ ColumnCannotBeSet (some /\ more)

else instance ColumnCannotBeSet t

-- | Slightly clearer type errors for missing columns
class MissingRequiredColumns (required ∷ Row Type) (inserted ∷ Row Type)

instance Union required e inserted ⇒ MissingRequiredColumns required inserted

into ∷
      ∀ tableName fields fieldNames fieldList required inserted.
      RowToList fields fieldList ⇒
      RequiredColumns fieldList required ⇒
      InsertList fields fieldNames inserted ⇒
      MissingRequiredColumns required inserted ⇒
      Table tableName fields →
      fieldNames →
      Insert →
      Into tableName fields fieldNames
into _ fieldNames _ = Into fieldNames

data Values fieldValues q = Values fieldValues q

data DefaultValues = DefaultValues

class InsertValues (fields ∷ Row Type) (names ∷ Type) (t ∷ Type)

-- | Multiple values, single column
instance InsertValues fields (Proxy name) u ⇒ InsertValues fields (Proxy name) (Array u)

-- | DEFAULT
else instance (Cons name t e fields, IsDefault t name) ⇒ InsertValues fields (Proxy name) Default

-- | Values
else instance
      ( UnwrapDefinition t u
      , Cons name t e fields
      , ToValue u
      ) ⇒
      InsertValues fields (Proxy name) u

-- | Column list
else instance (InsertValues fields name value, InsertValues fields some more) ⇒ InsertValues fields (name /\ some) (value /\ more)

-- | Multiple values, many columns
else instance (InsertValues fields (name /\ some) (value /\ more)) ⇒ InsertValues fields (name /\ some) (Array (value /\ more))

-- | Clearer error message in case of misplaced default value
class IsDefault (t ∷ Type) (name ∷ Symbol) | t → name

instance IsDefault constraints name ⇒ IsDefault (Column t constraints) name

else instance (IsDefault some name, IsDefault more name) ⇒ IsDefault (some /\ more) name

else instance IsDefault Default name

else instance
      ( Append "Column " name start
      , Append start " does not have a DEFAULT constraint" message
      , Fail (Text message)
      ) ⇒
      IsDefault t name

-- | Makes sure single and multiple insert syntax is not mixed
class ConsistentArity (values ∷ Type)

-- | Multiple values, single column
instance ConsistentArity (Array t)

-- | Multiple values, many columns
else instance MultipleInsert u ⇒ ConsistentArity (Array t /\ u)

-- | Column list
else instance SingleInsert u ⇒ ConsistentArity (t /\ u)

-- | Single value
else instance ConsistentArity t

class MultipleInsert (t ∷ Type)

instance MultipleInsert (Array t)

else instance MultipleInsert t ⇒ MultipleInsert (Array t /\ u)

else instance Fail (Text "Multiple INSERT value list must be array") ⇒ MultipleInsert u

class SingleInsert (t ∷ Type)

instance SingleInsert t ⇒ SingleInsert (t /\ u)

else instance Fail (Text "Single INSERT value list must not be array") ⇒ SingleInsert (Array t)

else instance SingleInsert t

values ∷
      ∀ tableName fields names fieldValues.
      ConsistentArity fieldValues ⇒
      InsertValues fields names fieldValues ⇒
      fieldValues →
      Into tableName fields names →
      Values fieldValues (Into tableName fields names)
values fieldValues q = Values fieldValues q

defaultValues ∷ DefaultValues
defaultValues = DefaultValues

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

data Update (name ∷ Symbol) (fields ∷ Row Type) = Update

data Set pairs q = Set pairs q

class ToUpdatePairs (fields ∷ Row Type) (pairs ∷ Type)

instance (Cons name t e fields, IsDefault t name) ⇒ ToUpdatePairs fields (Op (Proxy name) Default)

else instance
      ( Cons name t e fields
      , ColumnCannotBeSet t
      , UnwrapDefinition t u
      , ToValue u
      ) ⇒
      ToUpdatePairs fields (Op (Proxy name) u)

instance
      ( ToUpdatePairs fields head
      , ToUpdatePairs fields tail
      ) ⇒
      ToUpdatePairs fields (head /\ tail)

update ∷ ∀ name fields. Table name fields → Update name fields
update _ = Update

set ∷ ∀ name fields pairs. ToUpdatePairs fields pairs ⇒ pairs → Update name fields → Set pairs (Update name fields)
set pairs u = Set pairs u

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
data Delete = Delete

delete ∷ Delete
delete = Delete

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

data Returning fieldNames q = Returning fieldNames q

-- | RETURNING can only follow INSERT
class ToReturning (q ∷ Type) (fields ∷ Row Type) | q → fields

instance ToReturning (Values v (Into name fields fieldNames)) fields

else instance ToReturning (Into name fields DefaultValues) fields

else instance Fail (Text "RETURNING must follow INSERT ... VALUES or INSERT ... DEFAULT VALUES") ⇒ ToReturning q fields

-- | RETURNING statement
returning ∷ ∀ f q fields. ToReturning q fields ⇒ ReturningColumns f fields ⇒ f → q → Returning f q
returning f q = Returning f q

class ReturningColumns (f ∷ Type) (fields ∷ Row Type) | f → fields

instance Cons name t e fields ⇒ ReturningColumns (Proxy name) fields

instance (ReturningColumns a fields, ReturningColumns b fields) ⇒ ReturningColumns (a /\ b) fields

------------------------Projection machinery---------------------------

-- | A `RowList` of `Symbol`s
type SymbolList = RowList Symbol

-- | Computes SELECT projection as a `Row Type`
class ToProjection (s ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList) (projection ∷ Row Type) | s → fields projection

-- | Columns
instance (RowToList fields list, ColumnInScope name list t, Cons name t () projection) ⇒ ToProjection (Proxy name) fields aliases projection

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

-- | Aliased int literal
else instance Cons alias Int () projection ⇒ ToProjection (As alias Int) fields aliases projection

-- | Aliased boolean literal
else instance Cons alias Boolean () projection ⇒ ToProjection (As alias Boolean) fields aliases projection

-- | Aliased boolean literal
else instance Cons alias Boolean () projection ⇒ ToProjection (As alias (Op one another)) fields aliases projection

-- | Aliased aggregation
else instance Cons alias t () projection ⇒ ToProjection (As alias (Aggregate inp rest fields t)) fields aliases projection

-- | Aliased function
else instance Cons alias t () projection ⇒ ToProjection (As alias (PgFunction inp args fields t)) fields aliases projection

-- | Aliased column
else instance (RowToList fields list, ColumnInScope name list t, Cons alias t () projection) ⇒ ToProjection (As alias (Proxy name)) fields aliases projection

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

-- | DISTINCT
else instance ToProjection s fields aliases projection ⇒ ToProjection (Distinct s) fields aliases projection

-- | Aliased subquery as column
else instance
      ( SelectQuery q s f fields p
      --does source has an alias? we need it to figure out which qualified fields can be validated here
      , SourceAlias f table
      , SymbolListSingleton table single
      , ToProjection s fields single pro
      , RowToList pro list
      --is column already Maybe or needs to be made into Maybe? subquery can only return a single column
      , ToSingleColumn list name t
      --query is aliased so we have to use the alias instead of the column name
      , Cons alias t () projection
      ) ⇒
      ToProjection (As alias q) fd aliases projection

-- | Subquery as column
else instance
      ( SelectQuery q s f fields p
      , SourceAlias f table
      , SymbolListSingleton table single
      , ToProjection s fields single pro
      , RowToList pro list
      , ToSingleColumn list name t
      , Cons name t () projection
      ) ⇒
      ToProjection q fd aliases projection

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

instance (Cons fullPath t d fields, UnwrapNullable t v) ⇒ QualifiedColumn False fullPath fields v

--not required but makes for clearer type errors
class ToSingleColumn (fields ∷ RowList Type) (name ∷ Symbol) (t ∷ Type) | fields → name t

instance ToSingleColumn (Cons name (Maybe t) Nil) name (Maybe t)

else instance ToSingleColumn (Cons name t Nil) name (Maybe t)

-- | Query projections should not repeat column names
class UniqueColumnNames (some ∷ Row Type) (more ∷ Row Type)

instance UniqueColumnNames fields fields

-- | Joined sources must have unique names (alias or, for unaliased tables, the table name)
-- |
-- | Compares the list of names with its deduplicated version
class UniqueAliases (some ∷ SymbolList) (more ∷ SymbolList)

instance UniqueAliases aliases aliases

else instance Fail (Text "JOIN sources must have unique names. Alias tables that are joined more than once, e.g. users # as @\"u\"") ⇒ UniqueAliases some more

-- | Table/subquery alias or `Empty`
class SourceAlias (f ∷ Type) (alias ∷ Symbol) | f → alias

instance SourceAlias (As alias q) alias

-- | Unaliased tables can be qualified by their name
else instance SourceAlias (Table name fields) name

else instance SourceAlias f Empty

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
      , Union extended projection nubbed
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

-- | Column types of a table definition, without `Column` wrappers
-- |
-- | Statements only see these plain types (nullable columns are `Maybe`); `Column` constraints are only needed for INSERT, UPDATE and DDL
class ColumnTypes (list ∷ RowList Type) (columns ∷ Row Type) | list → columns

instance ColumnTypes Nil ()

instance
      ( UnwrapDefinition t u
      , Cons name u () head
      , ColumnTypes rest tail
      , Union head tail columns
      ) ⇒
      ColumnTypes (Cons name t rest) columns

-- | Computes all source fields with their alias
class QualifiedColumns (list ∷ RowList Type) (alias ∷ Symbol) (fields ∷ Row Type) | list alias → fields

instance QualifiedColumns Nil alias ()

instance
      ( ToPath alias path
      , Append path name fullPath
      , Cons fullPath t () head
      , QualifiedColumns rest alias tail
      , Union head tail fields
      ) ⇒
      QualifiedColumns (Cons name t rest) alias fields

-- | Optionally add source field alias
class ToPath (alias ∷ Symbol) (path ∷ Symbol) | alias → path

instance ToPath Empty Empty

else instance Append alias Dot path ⇒ ToPath alias path

-- | Creates a `SymbolList` single with a single entry
class SymbolListSingleton (alias ∷ Symbol) (list ∷ SymbolList) | alias → list

instance SymbolListSingleton alias (Cons alias alias Nil)

-- | Given a source `f`, compute its (non and qualified) fields
class SourceColumns (f ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList) | f → fields aliases

-- | Tables, whose columns can also be qualified by the table name
instance
      ( RowToList fields list
      , ColumnTypes list columns
      , RowToList columns columnList
      , QualifiedColumns columnList name qualified
      , Union qualified columns all
      , SymbolListSingleton name single
      ) ⇒
      SourceColumns (Table name fields) all single

-- | Aliased tables
else instance
      ( RowToList source list
      , ColumnTypes list columns
      , RowToList columns columnList
      , QualifiedColumns columnList alias aliased
      , Union aliased columns fields
      , SymbolListSingleton alias single
      ) ⇒
      SourceColumns (As alias (Table name source)) fields single

-- | Aliased subqueries
else instance
      ( SelectQuery q s f fd projection
      , RowToList projection list
      , QualifiedColumns list alias aliased
      , Union projection aliased fields
      , SymbolListSingleton alias single
      ) ⇒
      SourceColumns (As alias q) fields single

-- | Subqueries must be aliased
else instance Fail (Text "Expected query to end in AS statement") ⇒ SourceColumns q fields aliases

---------------------------Table machinery------------------------------------------

-- | TABLE clause for CREATE, ALTER and DROP
data T (t ∷ Type) q = T q

-- | Acceptable table operations
class ToTable (q ∷ Type) (t ∷ Type)

-- | CREATE TABLE
instance TableChecks (Table name columns) ⇒ ToTable Create (Table name columns)

-- | ALTER TABLE
else instance ToTable Alter (Table name fields)

-- | DROP TABLE
else instance ToTable Drop (Table name fields)

else instance Fail (Text "TABLE must follow CREATE, ALTER or DROP") ⇒ ToTable q t

-- | Table up for modification
table ∷ ∀ name q fields. ToTable q (Table name fields) ⇒ Table name fields → q → T (Table name fields) q
table _ q = T q

---------------------------CREATE------------------------------------------

data Create = Create

-- | CREATE ...
create ∷ Create
create = Create

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

-- | Reused by CREATE and ALTER
class TableChecks (t ∷ Type)

instance
      ( RowToList columns columnList
      , ValidColumnNames columnList
      , ValidConstraints columnList
      , ConstraintsToRowList columnList list
      , ValidComposites name list
      ) ⇒
      TableChecks (Table name columns)

-- | Fails on repeated column names
class ValidColumnNames (columns ∷ RowList Type)

instance
      ( ColumnNames columns names
      , RowListNub names nubbed
      , UniqueTableColumnNames names nubbed
      ) ⇒
      ValidColumnNames columns

-- | List of all column names in a table
class ColumnNames (columns ∷ RowList Type) (names ∷ SymbolList) | columns → names

instance ColumnNames Nil Nil

instance
      ( SymbolListSingleton name head
      , ColumnNames rest tail
      , RowListAppend head tail all
      ) ⇒
      ColumnNames (Cons name t rest) all

-- |
class UniqueTableColumnNames (some ∷ SymbolList) (more ∷ SymbolList)

instance UniqueTableColumnNames columns columns

-- | Fail on invalid constraints
class ValidConstraints (columns ∷ RowList Type)

instance ValidConstraints Nil

instance
      ( UniqueConstraints name constraints
      , MatchingForeignKey t constraints
      , SingleTypeComposite constraints
      , ValidNullableConstraints name t constraints
      , ValidConstraints rest
      ) ⇒
      ValidConstraints (Cons name (Column t constraints) rest)

else instance (ColumHasType t, ValidConstraints rest) ⇒ ValidConstraints (Cons name t rest)

-- | Constraints must be unique per column
class UniqueConstraints (name ∷ Symbol) (constraints ∷ Type)

instance (IsRepeated name some more, UniqueConstraints name more) ⇒ UniqueConstraints name (some /\ more)

else instance UniqueConstraints name t

-- | If a constraint appears more than once in a column
class IsRepeated (name ∷ Symbol) (t ∷ Type) (constraints ∷ Type)

instance (IsRepeated name t some, IsRepeated name t more) ⇒ IsRepeated name t (some /\ more)

else instance Fail (Beside (Beside (Text "Constraint ") (Quote t)) (Beside (Text " declared more than once for column ") (QuoteLabel name))) ⇒ IsRepeated name t t

else instance IsRepeated name t s

-- | Constraints must be paired with `Column`
class ColumHasType (column ∷ Type)

-- | For ALTER
instance ColumHasType (Proxy t)

else instance ToType t ⇒ ColumHasType t

-- |
class SingleTypeComposite (constraints ∷ Type)

instance (SingleTypeComposite t, SingleTypeComposite s) ⇒ SingleTypeComposite (t /\ s)

else instance Fail (Beside (Beside (Text "Composite ") (QuoteLabel name)) (Text " must not mix constraint types")) ⇒ SingleTypeComposite (Constraint (Composite name) (t /\ s))

else instance SingleTypeComposite t

-- | Foreign key name and type must match
class MatchingForeignKey (t ∷ Type) (constraints ∷ Type)

instance (MatchingForeignKey t some, MatchingForeignKey t more) ⇒ MatchingForeignKey t (some /\ more)

else instance (Cons name s e fields, UnwrapDefinition s t) ⇒ MatchingForeignKey t (ForeignKey name (Table n fields))

else instance MatchingForeignKey t s

-- | Nullable constraints can only be unique or foreign key
class ValidNullableConstraints (name ∷ Symbol) (t ∷ Type) (constraints ∷ Type)

instance IsValidNullableConstraint name constraints ⇒ ValidNullableConstraints name (Maybe t) constraints

else instance ValidNullableConstraints name t constraints

class IsValidNullableConstraint (name ∷ Symbol) (constraints ∷ Type)

instance (IsValidNullableConstraint name some, IsValidNullableConstraint name more) ⇒ IsValidNullableConstraint name (some /\ more)

else instance IsValidNullableConstraint name Unique

else instance IsValidNullableConstraint name (ForeignKey n f)

else instance Fail (Beside (Beside (Beside (Text "Nullable column ") (QuoteLabel name)) (Beside (Text " cannot have ") (Quote t))) (Text " constraint")) ⇒ IsValidNullableConstraint name t

-- | Flatten constraints into a list
class ConstraintsToRowList (source ∷ RowList Type) (constraints ∷ RowList Type) | source → constraints

instance ConstraintsToRowList Nil Nil

instance
      ( IncludeConstraint name constraints head
      , ConstraintsToRowList rest tail
      , RowListAppend head tail all
      ) ⇒
      ConstraintsToRowList (Cons name (Column t constraints) rest) all

else instance ConstraintsToRowList rest all ⇒ ConstraintsToRowList (Cons name r rest) all

-- | Constraints that have to be checked across columns
class IncludeConstraint (name ∷ Symbol) (constraints ∷ Type) (list ∷ RowList Type) | constraints → list

instance
      ( IncludeConstraint fn c head
      , IncludeConstraint fn rest tail
      , RowListAppend head tail all
      ) ⇒
      IncludeConstraint fn (c /\ rest) all

else instance IncludeConstraint columnName (Constraint (Composite name) t) (Cons name (C columnName t) Nil)

else instance IncludeConstraint fn (Constraint name t) (Cons name t Nil)

else instance IncludeConstraint fn PrimaryKey (Cons "0pk" PrimaryKey Nil)

else instance IncludeConstraint fn t Nil

-- | Check constraints across columns
class ValidComposites (columnName ∷ Symbol) (constraints ∷ RowList Type)

instance
      ( CheckComposite tableName name t rest
      , ValidComposites tableName rest
      ) ⇒
      ValidComposites tableName (Cons name t rest)

instance ValidComposites tn Nil

-- |
class CheckComposite (tableName ∷ Symbol) (name ∷ Symbol) (c ∷ Type) (rest ∷ RowList Type)

instance CheckComposite tn cn c Nil

instance Fail (Beside (Text "Table ") (Beside (QuoteLabel tableName) (Text " has duplicated primary keys. You may want to use a named composite constraint."))) ⇒ CheckComposite tableName cn PrimaryKey (Cons n PrimaryKey rest)

--composites are changed into C columnName constraintType
--we need to spare same type constraints and same table foreign keys
else instance CheckComposite tableName name (C cn t) (Cons name (C cn2 t) rest)

else instance CheckComposite tableName name (C cn (ForeignKey rcn table)) (Cons name (C cn2 (ForeignKey rcn2 table)) rest)

else instance Fail (Beside (Beside (Text "Constraint ") (QuoteLabel name)) (Beside (Text " declared more than once for table ") (QuoteLabel tableName))) ⇒ CheckComposite tableName name s (Cons name t rest)

else instance CheckComposite tn name t rest ⇒ CheckComposite tn name t (Cons n s rest)

---------------------------ALTER------------------------------------------

data Alter = Alter

-- ALTER ...
alter ∷ Alter
alter = Alter

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

{-
full alter table syntax supported by droplet
ALTER TABLE table_definition
ADD column_definition
-}

---------------------------ADD--------------------------------------------

data Add (name ∷ Symbol) column q = Add column q

-- | ALTER table ADD objects
class ToAdd (q ∷ Type)

instance ToAdd (Column t constraints)

instance ToAdd (Proxy q)

add ∷
      ∀ q name object columns extended.
      ToAdd q ⇒
      Cons object q columns extended ⇒
      TableChecks (Table name extended) ⇒
      Proxy object →
      q →
      T (Table name columns) Alter →
      Add object q (T (Table name columns) Alter)
add _ column t = Add column t

---------------------------DROP------------------------------------------

data Drop = Drop

-- | DROP ...
drop ∷ Drop
drop = Drop

---------------------------TABLE------------------------------------------

{-
full drop table supported by postgresql (https://www.postgresql.org/docs/current/sql-droptable.html)

DROP TABLE [ IF EXISTS ] name [, ...] [ CASCADE | RESTRICT ]

-}

{-
full drop table syntax supported by droplet
DROP TABLE table_definition
where table_definition is the Table type
-}
