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
      , class UnwrapAll
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
      , class Resume
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
      , class QueryMustBeAliased
      , class UniqueSources
      , class OuterScopeAlias
      , class OnCondition
      , class QueryOptionallyAliased
      , class ToJoin
      , class QualifiedColumn
      , class OnComparision
      , class ValidGroupByProjection
      , class GroupedColumns
      , class ToGroupBy
      , class ToOuterColumns
      , class ToWhere
      , class JoinedToMaybe
      , class CompatibleProjection
      , class ToUnion
      , class RequiredColumns
      , class ToAs
      , class ToFrom
      , class GroupBySource
      , class InsertList
      , class InsertValues
      , class ToPrepare
      , class ToProjection
      , class ToSelect
      , class ToSingleColumn
      , class ToSubExpression
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
      , resume
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
import Data.Reflectable (class Reflectable, class Reifiable)
import Data.Reflectable as DR
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Condition (class ToCondition, class ValidComparision, Exists(..), Op(..), OuterScope)
import Droplet.Language.Internal.Definition (class AppendPath, class ToType, class ToValue, class UnwrapDefinition, class UnwrapNullable, C, Column, Composite, Constraint, Default, Dot, E(..), Empty, ForeignKey, Identity, Joined, Path, PrimaryKey, Star, Table(..), Unique)
import Droplet.Language.Internal.Function (class ToArrayAgg, class ToStringAgg, Aggregate, PgFunction)
import Prim.Boolean (False, True)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Beside, Quote, QuoteLabel, Text)
import Type.Data.Boolean (class And, class If)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow, class RowListAppend, class RowListNub)

----------------------PREPARE----------------------------

data Prepare q = Prepare q Plan

-- | Name of this prepared statement
newtype Plan = Plan String

-- | Only complete statements are accepted by PREPARE
class ToPrepare (q ∷ Type)

instance ToPrepare (Select s p (From f fields rest))

instance ToPrepare (Insert (Into name fields fieldNames (Values v rest)))

instance ToPrepare (Update name fields (Set v rest))

instance ToPrepare (Delete (From f fields rest))

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

-- | SELECT representation. `projection` refers to the final output of this statement
data Select s (projection ∷ Row Type) rest = Select s rest

-- | Acceptable column types for SELECT statements
class ToSelect (s ∷ Type)

instance ToSelect (Proxy name)

instance ToSelect (Path table name)

instance ToSelect (As alias Int)

instance ToSelect (As alias (Proxy name))

instance ToSelect (As alias (Path table name))

instance ToSelect (As alias (Aggregate inp rest fields out))

instance ToSelect (As alias (PgFunction inp arg fields out))

instance (ToSelect r, ToSelect t) ⇒ ToSelect (r /\ t)

instance ToSelect Star

instance ToSelect (Distinct s)

instance (ToSubExpression f, LimitedResults rest) ⇒ ToSelect (Select f projection rest)

-- | Only single columns can be projected by subqueries
class ToSubExpression (f ∷ Type)

instance ToSubExpression (Proxy name)

instance ToSubExpression (Path table name)

instance ToSubExpression (As alias Int)

instance ToSubExpression (As alias (Proxy name))

instance ToSubExpression (As alias (Path table name))

instance ToSubExpression (As alias (PgFunction inp arg fields out))

instance ToSubExpression (As alias (Aggregate inp r fields out))

instance Fail (Text "Subqueries must return a single column") ⇒ ToSubExpression (a /\ b)

-- | Subqueries must return a single result
class LimitedResults (q ∷ Type)

instance LimitedResults rest ⇒ LimitedResults (From f fields rest)

instance LimitedResults rest ⇒ LimitedResults (Where c rest)

instance LimitedResults rest ⇒ LimitedResults (GroupBy f rest)

instance LimitedResults rest ⇒ LimitedResults (OrderBy f rest)

instance LimitedResults rest ⇒ LimitedResults (Offset rest)

instance LimitedResults (Limit 1 rest)

instance Fail (Text "Subqueries must return zero or one rows. Are you missing ORDER BY ... LIMIT 1?") ⇒ LimitedResults E

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

--as it is, From does not try to carry all source fields (for example qualified and unqualified columns from table as alias) because it makes projection code a lot harder for joins and select *
-- this can change if a simpler design is found
data From f (fields ∷ Row Type) rest = From f rest

-- | Acceptable sources for FROM statements
class ToFrom (f ∷ Type) (q ∷ Type) (fields ∷ Row Type) | q f → fields

-- | (DELETE) FROM table
instance ToFrom (Table name fields) (Delete E) fields

-- | FROM ... JOIN ...
else instance
      ( ToProjection s fields aliases selected
      , Nub selected unique
      , UniqueColumnNames selected unique
      ) ⇒
      ToFrom (Join k fields l r aliases (On c rest)) (Select s unique E) fields

-- | Anything `SourceColumns` can compute
else instance
      ( SourceColumns f fields aliases
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
      , ToOuterColumns list out
      , Union left out all
      , Nub all fields
      , RowListAppend las ras aliases
      ) ⇒
      ToJoin (Join Outer fd l r a (On c rest)) fields aliases

else instance SourceColumns q fields aliases ⇒ ToJoin q fields aliases

-- | OUTER JOINs make one side nullable, as a corresponding record may not be found
-- |
-- | For ease of use, this class marks the nullable side fields with `Joined`, later on `ToProjection` will flatten it to `Maybe`
class ToOuterColumns (list ∷ RowList Type) (fields ∷ Row Type) | list → fields

instance ToOuterColumns Nil ()

-- | Avoid nesting `Joined`s
instance
      ( Cons name (Joined t) () head
      , ToOuterColumns rest tail
      , Union head tail all
      ) ⇒
      ToOuterColumns (Cons name (Joined t) rest) all

else instance
      ( Cons name (Joined t) () head
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
      ToOuterColumns list out ⇒
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
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      OnComparision (Proxy name) fields aliases v

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
on ∷ ∀ k l r c fields aliases. OnCondition c fields aliases ⇒ c → Join k fields l r aliases E → Join k fields l r aliases (On c E)
on c (Join q r _) = Join q r $ On c E

-------------------------------WHERE----------------------------

data Where c rest = Where c rest

-- | WHERE can only follow FROM, UPDATE and DELETE
class ToWhere (c ∷ Type) (q ∷ Type)

instance (SourceAlias f alias, ToCondition c fields alias) ⇒ ToWhere c (Select s projection (From f fields E))

instance ToCondition c fields Empty ⇒ ToWhere c (Update name fields (Set v E))

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
class ToGroupBy (q ∷ Type) (s ∷ Type) (columns ∷ Row Type) | q → s columns

instance GroupBySource f columns ⇒ ToGroupBy (Select s p (From f fd E)) s columns

instance GroupBySource f columns ⇒ ToGroupBy (Select s p (From f fd (Where cond E))) s columns

-- |
class GroupBySource (f ∷ Type) (columns ∷ Row Type) | f → columns

--refactor: could be the same as tojoin if joins accepted non aliased tables
instance GroupBySource (Table name columns) columns

instance
      ( RowToList columns list
      , QualifiedColumns list alias aliased
      , Union aliased columns all
      ) ⇒
      GroupBySource (As alias (Table name columns)) all

instance GroupBySource (Join k columns q r a rest) columns

instance
      ( QueryMustBeAliased rest alias
      , RowToList projection list
      , QualifiedColumns list alias aliased
      , Union aliased projection all
      ) ⇒
      GroupBySource (Select s projection (From f fd rest)) all

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
      ∀ f s q sql grouped columns.
      ToGroupBy q s columns ⇒
      GroupedColumns f columns grouped ⇒
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

instance ToAs (Table name fields) alias

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

-- | ORDER BY
class ToOrderBy (f ∷ Type) (q ∷ Type)

instance (SortColumnsSource s projection f columns available, SortColumns st available) ⇒ ToOrderBy st (Select s projection (From f columns E))

--only grouped columns are allowed here
instance
      ( GroupedColumns g columns grouped
      , Union projection grouped pg
      , Nub pg available
      , SortColumns st available
      ) ⇒
      ToOrderBy st (Select s projection (From f columns (GroupBy g E)))

instance (SortColumnsSource s projection f columns available, SortColumns st available) ⇒ ToOrderBy st (Select s projection (From f columns (Where cd E)))

instance
      ( GroupedColumns g columns grouped
      , Union projection grouped pg
      , Nub pg available
      , SortColumns st available
      ) ⇒
      ToOrderBy st (Select s projection (From f columns (Where cd (GroupBy g E))))

-- for aggregate/window functions
instance ToOrderBy (Proxy name) String

instance ToOrderBy (Path alias name) String

instance ToOrderBy (Proxy name) (Proxy otherName)

instance ToOrderBy (Path alias name) (Path otherAlias otherName)

--must be typed here
instance
      ( Cons name t e fields
      , Cons otherName v r fields
      , UnwrapDefinition v w
      ) ⇒
      ToArrayAgg (OrderBy (Proxy name) (Proxy otherName)) fields w

instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e fields
      , AppendPath otherAlias otherName otherFullPath
      , Cons otherFullPath v r fields
      , UnwrapDefinition v w
      ) ⇒
      ToArrayAgg (OrderBy (Path alias name) (Path otherAlias otherName)) fields w

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
orderBy ∷ ∀ f q sql. ToOrderBy f q ⇒ Resume q (OrderBy f E) sql ⇒ f → q → sql
orderBy f q = resume q $ OrderBy f E

instance (Cons name t e columns, Cons fd g h columns) ⇒ ToStringAgg (Proxy name) (OrderBy (Proxy fd) String) columns

instance Cons name t e columns ⇒ ToStringAgg (Path table fd) (OrderBy (Proxy name) String) columns

instance Cons name t e columns ⇒ ToStringAgg (Proxy name) (OrderBy (Path alias fd) String) columns

instance ToStringAgg (Path table name) (OrderBy (Path alias fd) String) columns

------------------------LIMIT---------------------------

data Limit (n ∷ Int) rest = Limit rest

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
limit ∷ ∀ n q sql. ToLimit q ⇒ Resume q (Limit n E) sql ⇒ Proxy n → q → sql
limit _ q = resume q $ Limit E

------------------------OFFSET---------------------------

data Offset rest = Offset Int rest

class ToOffset (q ∷ Type)

instance ToOffset (Select s projection (From fr fields (OrderBy f E)))

instance ToOffset (Select s projection (From fr fields (OrderBy f (Limit n E))))

instance ToOffset (Select s projection (From fr fields (GroupBy fg (OrderBy f E))))

instance ToOffset (Select s projection (From fr fields (GroupBy fg (OrderBy f (Limit n E)))))

instance ToOffset (Select s projection (From fr fields (Where cd (OrderBy f E))))

instance ToOffset (Select s projection (From fr fields (Where cd (OrderBy f (Limit n E)))))

instance ToOffset (Select s projection (From fr fields (Where cd (GroupBy fg (OrderBy f E)))))

instance ToOffset (Select s projection (From fr fields (Where cd (GroupBy fg (OrderBy f (Limit n E))))))

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

insert ∷ Insert E
insert = Insert E

data Into (name ∷ Symbol) (fields ∷ Row Type) fieldNames rest = Into fieldNames rest

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
      Insert E →
      Insert (Into tableName fields fieldNames E)
into _ fieldNames _ = Insert (Into fieldNames E)

data Values fieldValues rest = Values fieldValues rest

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
      Insert (Into tableName fields names E) →
      Insert (Into tableName fields names (Values fieldValues E))
values fieldValues (Insert (Into names _)) = Insert <<< Into names $ Values fieldValues E

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

newtype Update (name ∷ Symbol) (fields ∷ Row Type) rest = Update rest

data Set pairs rest = Set pairs rest

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

update ∷ ∀ name fields. Table name fields → Update name fields E
update _ = Update E

set ∷ ∀ name fields pairs. ToUpdatePairs fields pairs ⇒ pairs → Update name fields E → Update name fields (Set pairs E)
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

instance ReturningColumns f fields ⇒ ToReturning f (Insert (Into tn fields fn (Values fv E)))

instance ReturningColumns f fields ⇒ ToReturning f (Insert (Into tn fields DefaultValues E))

class ReturningColumns (f ∷ Type) (fields ∷ Row Type) | f → fields

instance Cons name t e fields ⇒ ReturningColumns (Proxy name) fields

instance (ReturningColumns a fields, ReturningColumns b fields) ⇒ ReturningColumns (a /\ b) fields

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
      , UnwrapDefinition v u
      , Cons name u () projection
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
      , UnwrapDefinition v u
      , Cons alias u () projection
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
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      QualifiedColumn False fullPath fields v

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

instance SourceAlias (As alias (Table name fields)) alias

else instance QueryOptionallyAliased rest Empty alias ⇒ SourceAlias (Select s p (From f fd rest)) alias

else instance SourceAlias f Empty

-- | Find this query's alias, or fail at compile time if query is not aliased
class QueryMustBeAliased (q ∷ Type) (alias ∷ Symbol) | q → alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (Where cd rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (GroupBy f rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (OrderBy f rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (Limit n rest) alias

instance QueryMustBeAliased rest alias ⇒ QueryMustBeAliased (Offset rest) alias

instance Fail (Text "Expected query to end in AS statement") ⇒ QueryMustBeAliased E alias

instance QueryMustBeAliased (As alias E) alias

-- | If this query is in the form of (SELECT ...) AS alias, return `alias`, otherwise keep `name`
class QueryOptionallyAliased (q ∷ Type) (name ∷ Symbol) (alias ∷ Symbol) | q → name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (Where cd rest) name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (GroupBy f rest) name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (OrderBy f rest) name alias

instance QueryOptionallyAliased rest name alias ⇒ QueryOptionallyAliased (Limit n rest) name alias

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
      ( UnwrapDefinition t u
      , Cons name u () head
      , UnwrapAll rest tail
      , Union head tail projection
      ) ⇒
      UnwrapAll (Cons name t rest) projection

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

-- | `Joined` fields appear as `Maybe` in projections
class JoinedToMaybe (t ∷ Type) (v ∷ Type) | t → v

instance JoinedToMaybe (Joined (f (Maybe t))) (Maybe t)

else instance JoinedToMaybe (Joined (Maybe t)) (Maybe t)

else instance UnwrapDefinition t u ⇒ JoinedToMaybe (Joined t) (Maybe u)

else instance JoinedToMaybe t t

-- | Creates a `SymbolList` single with a single entry
class SymbolListSingleton (alias ∷ Symbol) (list ∷ SymbolList) | alias → list

instance SymbolListSingleton alias (Cons alias alias Nil)

-- | Given a source `f`, compute its (non and qualified) fields
class SourceColumns (f ∷ Type) (fields ∷ Row Type) (aliases ∷ SymbolList) | f → fields aliases

-- | Tables
instance SourceColumns (Table name fields) fields Nil

-- | Aliased tables
instance
      ( RowToList source list
      , QualifiedColumns list alias aliased
      , Union aliased source fields
      , SymbolListSingleton alias single
      ) ⇒
      SourceColumns (As alias (Table name source)) fields single

-- | Aliased subqueries
instance
      ( QueryMustBeAliased rest alias
      , RowToList projection list
      , QualifiedColumns list alias aliased
      , Union projection aliased fields
      , SymbolListSingleton alias single
      ) ⇒
      SourceColumns (Select s projection (From f fd rest)) fields single

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

else instance Resume rest b c ⇒ Resume (Limit n rest) b (Limit n c) where
      resume (Limit rest) b = Limit $ resume rest b

else instance Resume rest b c ⇒ Resume (Offset rest) b (Offset c) where
      resume (Offset n rest) b = Offset n $ resume rest b

else instance Resume rest b c ⇒ Resume (Update n f rest) b (Update n f c) where
      resume (Update rest) b = Update $ resume rest b

else instance Resume rest b c ⇒ Resume (Insert rest) b (Insert c) where
      resume (Insert rest) b = Insert $ resume rest b

else instance Resume rest b c ⇒ Resume (Into n f fd rest) b (Into n f fd c) where
      resume (Into f rest) b = Into f $ resume rest b

else instance Resume rest b c ⇒ Resume (Values v rest) b (Values v c) where
      resume (Values v rest) b = Values v $ resume rest b

else instance Resume rest b c ⇒ Resume (Set p rest) b (Set p c) where
      resume (Set p rest) b = Set p $ resume rest b

else instance Resume rest b c ⇒ Resume (Delete rest) b (Delete c) where
      resume (Delete rest) b = Delete $ resume rest b

else instance Resume (As alias E) b (As alias b) where
      resume (As _) b = As b

else instance Resume rest b c ⇒ Resume (Create rest) b (Create c) where
      resume (Create rest) b = Create $ resume rest b

else instance Resume rest b c ⇒ Resume (Drop rest) b (Drop c) where
      resume (Drop rest) b = Drop $ resume rest b

else instance Resume rest b c ⇒ Resume (Alter rest) b (Alter c) where
      resume (Alter rest) b = Alter $ resume rest b

else instance Resume E b b where
      resume _ b = b

else instance Resume b a c ⇒ Resume a b c where
      resume a b = resume b a

---------------------------Table machinery------------------------------------------

-- | Helper data type for extending TABLE syntax
newtype T (t ∷ Type) rest = T rest

-- | Acceptable table operations
class ToTable (q ∷ Type) (t ∷ Type) | q → t

-- | Table up for modification
table ∷ ∀ name q fields sql. ToTable q (Table name fields) ⇒ Resume q (Table name fields) sql ⇒ Table name fields → q → sql
table _ q = resume q Table

---------------------------CREATE------------------------------------------

newtype Create rest = Create rest

-- | CREATE ...
create ∷ Create E
create = Create E

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

-- | CREATE TABLE
instance TableChecks (Table name columns) ⇒ ToTable (Create E) (Table name columns)

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

newtype Alter rest = Alter rest

-- ALTER ...
alter ∷ Alter E
alter = Alter E

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

instance ToTable (Alter E) (Table name fields)

---------------------------ADD--------------------------------------------

newtype Add (name ∷ Symbol) rest = Add rest

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
      Alter (Table name columns) →
      Alter (T (Table name columns) (Add object q))
add _ column (Alter _) = Alter <<< T $ Add column

---------------------------DROP------------------------------------------

newtype Drop rest = Drop rest

-- | DROP ...
drop ∷ Drop E
drop = Drop E

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

instance ToTable (Drop E) (Table name fields)