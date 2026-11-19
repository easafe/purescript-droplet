-- | `ToQuery`, a type class to generate parameterized SQL statement strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Driver` instead
module Droplet.Language.Internal.Translate
      ( Query(..)
      , class FilteredQuery
      , class TranslateFieldDefinition
      , class QualifiedProjection
      , class QualifiedSubQuery
      , class ToNakedProjection
      , class NakedSubQuery
      , class SingleQualifiedColumn
      , class ColumnAlias
      , class NoAggregations
      , class AggregatedQuery
      , class ToReferenceDefinition
      , class IsValidAggregation
      , class CompositeConstraints
      , class CompositeFields
      , class CompositeFieldList
      , class TranslateCompositeConstraint
      , class ToJoinType
      , class SingleColumn
      , class ToConstraintDefinition
      , class ToNullableDefinition
      , class ReferenceList
      , class ToCompositeConstraintDefinition
      , class ToQuery
      , class ToStatement
      , class ToColumns
      , class ToSource
      , class ToExpression
      , class ToExpressionList
      , class ToSetPairs
      , class ToRows
      , toStatement
      , toColumns
      , toSource
      , toExpression
      , toExpressionList
      , toSetPairs
      , toRows
      , toQuery
      , toConstraintDefinition
      , toCompositeConstraintDefinition
      , toReferenceDefinition
      , toNullableDefinition
      , compositeFieldList
      , translateCompositeConstraint
      , toJoinType
      , translateFieldDefinition
      , buildQuery
      , unsafeBuildQuery
      ) where

import Prim hiding (Constraint)

import Data.Array ((..), (:))
import Data.Array as DA
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as DAN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Reflectable (class Reflectable)
import Data.Reflectable as DR
import Data.String as DST
import Data.String.Regex as DSR
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe as DSRU
import Data.Tuple (Tuple(..))
import Data.Tuple as DTP
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Language.Internal.Ast (Expression, JoinType, SortDirection, Source, Statement)
import Droplet.Language.Internal.Ast as DLIA
import Droplet.Language.Internal.Condition (Exists, In, IsNull, IsNotNull, Not, Op(..))
import Droplet.Language.Internal.Definition (class AppendPath, class IsNullable, class ToParameters, class ToType, class ToValue, class UnwrapNullable, C, Column, Composite, Constraint, Default, E, Empty, ForeignKey, Identity, Path, PrimaryKey, Star, Table, Unique)
import Droplet.Language.Internal.Definition as DLID
import Droplet.Language.Internal.Function (Aggregate(..), PgFunction(..))
import Droplet.Language.Internal.Syntax (class ColumnTypes, class ConstraintsToRowList, class IsSubQuery, class OnlyAggregations, class SelectQuery, class ToProjection, class ToReturning, class UniqueColumnNames, Add, Alter, As(..), Create, DefaultValues, Delete, Distinct(..), Drop, From(..), GroupBy(..), Inclusion(..), Inner, Into(..), Join(..), Limit(..), Offset(..), On(..), OrderBy(..), Outer, Plan, Prepare(..), Returning(..), Select(..), Set(..), Side, Sort(..), T, Union(..), Update, Values(..), Where(..))
import Droplet.Language.Internal.Token (array_aggFunctionName, atSymbol, closeBracket, comma, constraintKeyword, countFunctionName, dotSymbol, foreignKeyKeyword, identityKeyword, notNullKeyword, openBracket, parameterSymbol, primaryKeyKeyword, quoteSymbol, referencesKeyword, space, string_aggFunctionName, uniqueKeyword)
import Foreign (Foreign)
import Prelude (class Show, Unit, map, show, ($), (<$>), (<<<), (<>))
import Prim.Boolean (False, True)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Type.Data.Boolean (class And)
import Type.Proxy (Proxy(..))
import Type.RowList (class RowListRemove, class RowListSet)

data Query (projection ∷ Row Type) = Query (Maybe Plan) String (Array Foreign)

instance Show (Query projection) where
      show (Query _ q _) = q

-- | Builds a SQL query
-- |
-- | Statements are lowered to `Droplet.Language.Internal.Ast` and then printed
class ToQuery (q ∷ Type) (projection ∷ Row Type) | q → projection where
      toQuery ∷ q → Query projection

-- | "Naked" queries in the shape of SELECT ...
instance
      ( ToNakedProjection s projection
      , Nub projection unique
      , UniqueColumnNames projection unique
      , ToStatement (Select s)
      ) ⇒
      ToQuery (Select s) unique where
      toQuery q = fromStatement (toStatement q)

-- | UNION
else instance
      ( ToQuery q final
      , ToQuery r p
      , ToStatement (Union q r)
      ) ⇒
      ToQuery (Union q r) final where
      toQuery q = fromStatement (toStatement q)

-- | PREPARE
else instance ToQuery q projection ⇒ ToQuery (Prepare q) projection where
      toQuery (Prepare q plan) = case toQuery q of
            Query _ sql parameters → Query (Just plan) sql parameters

-- | INSERT ... RETURNING
else instance
      ( ToReturning q fields
      , RowToList fields list
      , ColumnTypes list columns
      , ToProjection f columns Nil projection
      , ToStatement (Returning f q)
      ) ⇒
      ToQuery (Returning f q) projection where
      toQuery q = fromStatement (toStatement q)

-- | INSERT DEFAULT VALUES
else instance ToStatement (Into name fields DefaultValues) ⇒ ToQuery (Into name fields DefaultValues) () where
      toQuery q = fromStatement (toStatement q)

-- | INSERT
else instance ToStatement (Values v (Into name fields fieldNames)) ⇒ ToQuery (Values v (Into name fields fieldNames)) () where
      toQuery q = fromStatement (toStatement q)

-- | UPDATE
else instance ToStatement (Set values (Update table fields)) ⇒ ToQuery (Set values (Update table fields)) () where
      toQuery q = fromStatement (toStatement q)

-- | UPDATE ... WHERE
else instance ToStatement (Where c (Set values (Update table fields))) ⇒ ToQuery (Where c (Set values (Update table fields))) () where
      toQuery q = fromStatement (toStatement q)

-- | DELETE
else instance ToStatement (From f fields projection Delete) ⇒ ToQuery (From f fields projection Delete) () where
      toQuery q = fromStatement (toStatement q)

-- | DELETE ... WHERE
else instance ToStatement (Where c (From f fields projection Delete)) ⇒ ToQuery (Where c (From f fields projection Delete)) () where
      toQuery q = fromStatement (toStatement q)

-- | CREATE TABLE
else instance ToStatement (T (Table name fields) Create) ⇒ ToQuery (T (Table name fields) Create) () where
      toQuery q = fromStatement (toStatement q)

-- | ALTER TABLE ... ADD
else instance ToStatement (Add n column (T (Table name fields) Alter)) ⇒ ToQuery (Add n column (T (Table name fields) Alter)) () where
      toQuery q = fromStatement (toStatement q)

-- | DROP TABLE
else instance ToStatement (T (Table name fields) Drop) ⇒ ToQuery (T (Table name fields) Drop) () where
      toQuery q = fromStatement (toStatement q)

-- | Unsafe queries
else instance ToQuery (Query projection) projection where
      toQuery q = q

-- | Prevents top level queries to end in AS
else instance Fail (Text "AS statement cannot be top level") ⇒ ToQuery (As alias q) projection where
      toQuery _ = Query Nothing "" []

-- | Fully formed queries in the shape of SELECT ... FROM ...
else instance
      ( SelectQuery q s f fields projection
      --aggregation errors
      , AggregatedQuery s q
      --on and where condition errors
      , FilteredQuery q fields
      --alias errors
      , QualifiedProjection s fields qual
      --qualified columns projection
      , Union qual projection all
      , Nub all final
      , ToStatement q
      ) ⇒
      ToQuery q final where
      toQuery q = fromStatement (toStatement q)

-- | Asserts that queries not using GROUP BY do not mix aggregated and non aggregated columns
class AggregatedQuery (s ∷ Type) (q ∷ Type)

instance AggregatedQuery s (GroupBy g q)

else instance AggregatedQuery s q ⇒ AggregatedQuery s (Where c q)

else instance AggregatedQuery s q ⇒ AggregatedQuery s (OrderBy o q)

else instance AggregatedQuery s q ⇒ AggregatedQuery s (Limit n q)

else instance AggregatedQuery s q ⇒ AggregatedQuery s (Offset q)

else instance AggregatedQuery s q ⇒ AggregatedQuery (Distinct s) q

else instance
      ( NoAggregations s no
      , OnlyAggregations s yes
      , IsValidAggregation no yes
      ) ⇒
      AggregatedQuery s q

-- | Are all columns not aggregated?
class NoAggregations (q ∷ Type) (is ∷ Boolean) | q → is

instance NoAggregations (As n (Aggregate i rest f o)) False

else instance
      ( NoAggregations a isa
      , NoAggregations b isb
      , And isa isb is
      ) ⇒
      NoAggregations (a /\ b) is

else instance NoAggregations s True

-- | Check aggregation results
-- |
-- | Having a separated type class leads to better error messages
class IsValidAggregation (s ∷ Boolean) (t ∷ Boolean)

instance Fail (Text "Projection cannot include aggregations. Are you missing a GROUP BY clause?") ⇒ IsValidAggregation False False

else instance IsValidAggregation s t

-- | Validates qualified columns that `ToProjection` didn't have the context for
class QualifiedProjection (s ∷ Type) (outer ∷ Row Type) (projection ∷ Row Type) | s → outer projection

instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , Cons fullPath t () projection
      ) ⇒
      QualifiedProjection (Path alias name) outer projection

else instance
      ( AppendPath table name fullPath
      , Cons fullPath t e outer
      , Cons alias t () projection
      ) ⇒
      QualifiedProjection (As alias (Path table name)) outer projection

else instance
      ( AppendPath table name fullPath
      , AppendPath tb nm ftn
      , Cons fullPath t e outer
      , Cons ftn s g outer
      , Cons alias out () projection
      ) ⇒
      QualifiedProjection (As alias (Aggregate (Path table name) (OrderBy (Path tb nm) rd) fd out)) outer projection

else instance
      ( AppendPath table name fullPath
      , Cons fullPath t d outer
      , Cons alias out () projection
      ) ⇒
      QualifiedProjection (As alias (Aggregate (Proxy n) (OrderBy (Path table name) rd) fd out)) outer projection

else instance
      ( AppendPath table name fullPath
      , Cons fullPath t e outer
      , Cons alias out () projection
      ) ⇒
      QualifiedProjection (As alias (Aggregate (Path table name) rest fd out)) outer projection

else instance
      ( QualifiedProjection s outer some
      , QualifiedProjection t outer more
      , Union some more projection
      ) ⇒
      QualifiedProjection (Tuple s t) outer projection

else instance QualifiedProjection s outer projection ⇒ QualifiedProjection (Distinct s) outer projection

else instance (IsSubQuery s is, QualifiedSubQuery is s outer projection) ⇒ QualifiedProjection s outer projection

-- | Subqueries have access to the outer scope, so their conditions and projections must be checked against it
class QualifiedSubQuery (is ∷ Boolean) (s ∷ Type) (outer ∷ Row Type) (projection ∷ Row Type) | is s → outer projection

instance QualifiedSubQuery False s outer ()

else instance
      ( SelectQuery q s f fields p
      , Union outer fields all
      , Nub all nubbed
      , FilteredQuery q nubbed
      , QualifiedProjection s nubbed projection
      , RowToList projection list
      , SingleQualifiedColumn list alias single
      ) ⇒
      QualifiedSubQuery True (As alias q) outer single

else instance
      ( SelectQuery q s f fields p
      , Union outer fields all
      , Nub all nubbed
      , FilteredQuery q nubbed
      , QualifiedProjection s nubbed projection
      , RowToList projection list
      , SingleQualifiedColumn list Empty single
      ) ⇒
      QualifiedSubQuery True q outer single

-- | Projects a single qualified column, if it exists
class SingleQualifiedColumn (fields ∷ RowList Type) (alias ∷ Symbol) (single ∷ Row Type) | fields alias → single

instance SingleQualifiedColumn RL.Nil alias ()

else instance (ColumnAlias alias name final, Cons final (Maybe t) () single) ⇒ SingleQualifiedColumn (RL.Cons name (Maybe t) RL.Nil) alias single

else instance (ColumnAlias alias name final, Cons final (Maybe t) () single) ⇒ SingleQualifiedColumn (RL.Cons name t RL.Nil) alias single

-- | Use the column name unless an alias was given
class ColumnAlias (alias ∷ Symbol) (name ∷ Symbol) (final ∷ Symbol) | alias name → final

instance ColumnAlias Empty name name

else instance ColumnAlias alias name alias

-- | would be nice to dry this like tocondition, but gotta find a way to only check `Path`s
-- | Checks for invalid qualified columns usage in conditional clauses
class FilteredQuery (q ∷ Type) (outer ∷ Row Type)

instance (FilteredQuery cond outer, FilteredQuery q outer) ⇒ FilteredQuery (Where cond q) outer

else instance FilteredQuery q outer ⇒ FilteredQuery (GroupBy g q) outer

else instance FilteredQuery q outer ⇒ FilteredQuery (OrderBy o q) outer

else instance FilteredQuery q outer ⇒ FilteredQuery (Limit n q) outer

else instance FilteredQuery q outer ⇒ FilteredQuery (Offset q) outer

else instance FilteredQuery f outer ⇒ FilteredQuery (From f fields projection q) outer

else instance FilteredQuery cond outer ⇒ FilteredQuery (On cond j) outer

else instance (FilteredQuery (Op a b) outer, FilteredQuery (Op c d) outer) ⇒ FilteredQuery (Op (Op a b) (Op c d)) outer

else instance FilteredQuery (Op a b) outer ⇒ FilteredQuery (Op Not (Op a b)) outer

else instance (AppendPath alias name fullPath, Cons fullPath t e outer, IsNullable t) ⇒ FilteredQuery (Op IsNotNull (Path alias name)) outer

else instance (AppendPath alias name fullPath, Cons fullPath t e outer, IsNullable t) ⇒ FilteredQuery (Op IsNull (Path alias name)) outer

else instance
      (
        -- exists support arbitrary queries, so we gotta repeat all of these....
        SelectQuery q s f fields p
      , AggregatedQuery s q
      , QualifiedProjection s outer o
      , Union outer fields os
      , Nub os allOut
      , FilteredQuery q allOut
      ) ⇒
      FilteredQuery (Op Exists q) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapNullable t v
      ) ⇒
      FilteredQuery (Op In (Op (Path alias name) (NonEmptyArray v))) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapNullable t w
      , AppendPath otherAlias otherName otherFullPath
      , Cons otherFullPath u f outer
      , UnwrapNullable u w
      ) ⇒
      FilteredQuery (Op (Path alias name) (Path otherAlias otherName)) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapNullable t w
      , Cons otherName u f outer
      , UnwrapNullable u w
      ) ⇒
      FilteredQuery (Op (Path alias name) (Proxy otherName)) outer

else instance
      ( Cons name t f outer
      , UnwrapNullable t w
      , AppendPath alias otherName fullPath
      , Cons fullPath u e outer
      , UnwrapNullable u w
      ) ⇒
      FilteredQuery (Op (Proxy name) (Path alias otherName)) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapNullable t v
      ) ⇒
      FilteredQuery (Op (Path alias name) v) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapNullable t v
      ) ⇒
      FilteredQuery (Op v (Path alias name)) outer

else instance FilteredQuery e outer

-- | Naked selects may be composed of subqueries whose projections need to be checked and merged individually
class ToNakedProjection (s ∷ Type) (projection ∷ Row Type)

instance
      ( ToNakedProjection s some
      , ToNakedProjection t more
      , Union some more projection
      ) ⇒
      ToNakedProjection (Tuple s t) projection

else instance ToNakedProjection s projection ⇒ ToNakedProjection (Distinct s) projection

else instance (IsSubQuery s is, NakedSubQuery is s projection) ⇒ ToNakedProjection s projection

-- | Subqueries in naked selects are necessarily correlated to nothing, but their own qualified columns still need checking
class NakedSubQuery (is ∷ Boolean) (s ∷ Type) (projection ∷ Row Type) | is s → projection

instance ToProjection s () Nil projection ⇒ NakedSubQuery False s projection

else instance
      ( --let ToProjection figure out alias and outer since this is necessarily a subquery
        ToProjection (As alias q) () Nil projection
      , SelectQuery q s f fields p
      , QualifiedProjection s fields refs
      ) ⇒
      NakedSubQuery True (As alias q) projection

else instance
      ( ToProjection q () Nil projection
      , SelectQuery q s f fields p
      , QualifiedProjection s fields refs
      ) ⇒
      NakedSubQuery True q projection

------------------------Lowering to Ast---------------------------

-- | Lower a type checked statement to its plain representation
-- |
-- | Statements are nested with the last clause outermost, so each instance lowers the inner statement and appends its own clause
class ToStatement q where
      toStatement ∷ q → Statement

-- | SELECT
instance ToColumns s ⇒ ToStatement (Select s) where
      toStatement (Select s) = [ DLIA.Select (toColumns s) ]

-- | DELETE
instance ToStatement Delete where
      toStatement _ = [ DLIA.Delete ]

-- | UPDATE
instance Reflectable name String ⇒ ToStatement (Update name fields) where
      toStatement _ = [ DLIA.Update (DR.reflectType (Proxy ∷ _ name)) ]

-- | SET
instance (ToStatement q, ToSetPairs pairs) ⇒ ToStatement (Set pairs q) where
      toStatement (Set pairs q) = DA.snoc (toStatement q) (DLIA.Set (toSetPairs pairs))

-- | FROM
instance (ToStatement q, ToSource f) ⇒ ToStatement (From f fields projection q) where
      toStatement (From f q) = DA.snoc (toStatement q) (DLIA.From (toSource f))

-- | WHERE
instance (ToStatement q, ToExpression c) ⇒ ToStatement (Where c q) where
      toStatement (Where c q) = DA.snoc (toStatement q) (DLIA.Where (toExpression c))

-- | GROUP BY
instance (ToStatement q, ToExpressionList f) ⇒ ToStatement (GroupBy f q) where
      toStatement (GroupBy f q) = DA.snoc (toStatement q) (DLIA.GroupBy (toExpressionList f))

-- | ORDER BY
instance (ToStatement q, ToExpressionList f) ⇒ ToStatement (OrderBy f q) where
      toStatement (OrderBy f q) = DA.snoc (toStatement q) (DLIA.OrderBy (toExpressionList f))

-- | LIMIT
instance (ToStatement q, Reflectable n Int) ⇒ ToStatement (Limit n q) where
      toStatement (Limit q) = DA.snoc (toStatement q) (DLIA.Limit (DR.reflectType (Proxy ∷ _ n)))

-- | OFFSET
instance ToStatement q ⇒ ToStatement (Offset q) where
      toStatement (Offset n q) = DA.snoc (toStatement q) (DLIA.Offset n)

-- | INSERT DEFAULT VALUES
instance Reflectable name String ⇒ ToStatement (Into name fields DefaultValues) where
      toStatement _ = [ DLIA.InsertDefaultValues (DR.reflectType (Proxy ∷ _ name)) ]

-- | INSERT ... VALUES
instance (Reflectable name String, ToExpressionList fieldNames, ToRows v) ⇒ ToStatement (Values v (Into name fields fieldNames)) where
      toStatement (Values v (Into fieldNames)) = [ DLIA.InsertInto (DR.reflectType (Proxy ∷ _ name)) (toExpressionList fieldNames), DLIA.Values (toRows v) ]

-- | RETURNING
instance (ToStatement q, ToExpressionList f) ⇒ ToStatement (Returning f q) where
      toStatement (Returning f q) = DA.snoc (toStatement q) (DLIA.Returning (toExpressionList f))

-- | UNION
instance (ToStatement q, ToStatement r) ⇒ ToStatement (Union q r) where
      toStatement (Union inclusion q r) = [ DLIA.Union isAll (toStatement q) (toStatement r) ]
            where
            isAll = case inclusion of
                  All → true
                  Unique → false

-- | Lower SELECT projections
class ToColumns s where
      toColumns ∷ s → Array Expression

instance Reflectable name String ⇒ ToColumns (Proxy name) where
      toColumns _ = [ DLIA.Column (DR.reflectType (Proxy ∷ _ name)) ]

-- | Columns in the format alias.name must be aliased to avoid problems with ambiguous column names and *
else instance (Reflectable alias String, Reflectable name String) ⇒ ToColumns (Path alias name) where
      toColumns _ = [ DLIA.Labeled (qualifiedColumn (Proxy ∷ _ alias) (Proxy ∷ _ name)) (DR.reflectType (Proxy ∷ _ alias) <> dotSymbol <> DR.reflectType (Proxy ∷ _ name)) ]

else instance ToColumns Star where
      toColumns _ = [ DLIA.Star ]

else instance Reflectable alias String ⇒ ToColumns (As alias Int) where
      toColumns (As n) = [ DLIA.As (DLIA.Literal (show n)) (DR.reflectType (Proxy ∷ _ alias)) ]

else instance Reflectable alias String ⇒ ToColumns (As alias Boolean) where
      toColumns (As b) = [ DLIA.As (DLIA.Literal (show b)) (DR.reflectType (Proxy ∷ _ alias)) ]

else instance (Reflectable alias String, ToExpression (Op one another)) ⇒ ToColumns (As alias (Op one another)) where
      toColumns (As op) = [ DLIA.As (toExpression op) (DR.reflectType (Proxy ∷ _ alias)) ]

else instance (Reflectable alias String, ToExpression (Aggregate inp rest fields out)) ⇒ ToColumns (As alias (Aggregate inp rest fields out)) where
      toColumns (As agg) = [ DLIA.As (toExpression agg) (DR.reflectType (Proxy ∷ _ alias)) ]

else instance (Reflectable alias String, ToExpression (PgFunction inp args fields out)) ⇒ ToColumns (As alias (PgFunction inp args fields out)) where
      toColumns (As func) = [ DLIA.As (toExpression func) (DR.reflectType (Proxy ∷ _ alias)) ]

else instance (Reflectable alias String, Reflectable name String) ⇒ ToColumns (As alias (Proxy name)) where
      toColumns _ = [ DLIA.As (DLIA.Column (DR.reflectType (Proxy ∷ _ name))) (DR.reflectType (Proxy ∷ _ alias)) ]

else instance (Reflectable alias String, Reflectable table String, Reflectable name String) ⇒ ToColumns (As alias (Path table name)) where
      toColumns _ = [ DLIA.As (qualifiedColumn (Proxy ∷ _ table) (Proxy ∷ _ name)) (DR.reflectType (Proxy ∷ _ alias)) ]

else instance (ToColumns s, ToColumns t) ⇒ ToColumns (Tuple s t) where
      toColumns (Tuple s t) = toColumns s <> toColumns t

else instance ToColumns s ⇒ ToColumns (Distinct s) where
      toColumns (Distinct s) = [ DLIA.Distinct (toColumns s) ]

-- | Aliased subquery
else instance (Reflectable alias String, ToStatement q) ⇒ ToColumns (As alias q) where
      toColumns (As q) = [ DLIA.As (DLIA.SubQueryExpression (toStatement q)) (DR.reflectType (Proxy ∷ _ alias)) ]

-- | Subquery
else instance ToStatement q ⇒ ToColumns q where
      toColumns q = [ DLIA.SubQueryExpression (toStatement q) ]

-- | Lower FROM sources
class ToSource f where
      toSource ∷ f → Source

instance Reflectable name String ⇒ ToSource (Table name fields) where
      toSource _ = DLIA.Table (DR.reflectType (Proxy ∷ _ name))

else instance (Reflectable name String, Reflectable alias String) ⇒ ToSource (As alias (Table name fields)) where
      toSource _ = DLIA.AliasedTable (DR.reflectType (Proxy ∷ _ name)) (DR.reflectType (Proxy ∷ _ alias))

else instance (ToJoinType k, ToSource l, ToSource r, ToExpression c) ⇒ ToSource (On c (Join k fields l r a)) where
      toSource (On c (Join l r)) = DLIA.Join (toJoinType (Proxy ∷ _ k)) (toSource l) (toSource r) (toExpression c)

-- | Aliased subquery
else instance (Reflectable alias String, ToStatement q) ⇒ ToSource (As alias q) where
      toSource (As q) = DLIA.SubQuery (toStatement q) (DR.reflectType (Proxy ∷ _ alias))

class ToJoinType (k ∷ Side) where
      toJoinType ∷ Proxy k → JoinType

instance ToJoinType Inner where
      toJoinType _ = DLIA.InnerJoin

instance ToJoinType Outer where
      toJoinType _ = DLIA.LeftJoin

-- | Lower conditions, function arguments and other expressions
class ToExpression e where
      toExpression ∷ e → Expression

-- | EXISTS
instance ToStatement q ⇒ ToExpression (Op Exists q) where
      toExpression (Op _ _ q) = DLIA.Exists (toStatement q)

-- | IS NOT NULL
else instance ToExpression a ⇒ ToExpression (Op IsNotNull a) where
      toExpression (Op _ _ a) = DLIA.IsNotNull (toExpression a)

-- | IS NULL
else instance ToExpression a ⇒ ToExpression (Op IsNull a) where
      toExpression (Op _ _ a) = DLIA.IsNull (toExpression a)

-- | NOT
else instance ToExpression a ⇒ ToExpression (Op Not a) where
      toExpression (Op _ _ a) = DLIA.Not (toExpression a)

-- | IN
else instance (ToExpression a, ToExpression b) ⇒ ToExpression (Op In (Op a (NonEmptyArray b))) where
      toExpression (Op _ _ (Op _ a values)) = DLIA.In (toExpression a) (map toExpression (DAN.toArray values))

-- | Comparisons, AND, OR
else instance (ToExpression a, ToExpression b) ⇒ ToExpression (Op a b) where
      toExpression (Op operator a b) = DLIA.Operation operator (toExpression a) (toExpression b)

else instance Reflectable name String ⇒ ToExpression (Proxy name) where
      toExpression _ = DLIA.Column (DR.reflectType (Proxy ∷ _ name))

else instance (Reflectable alias String, Reflectable name String) ⇒ ToExpression (Path alias name) where
      toExpression _ = qualifiedColumn (Proxy ∷ _ alias) (Proxy ∷ _ name)

else instance ToExpression Star where
      toExpression _ = DLIA.Star

else instance Reflectable name String ⇒ ToExpression (Sort (Proxy name)) where
      toExpression s = DLIA.Sorted (DLIA.Column (DR.reflectType (Proxy ∷ _ name))) (sortDirection s)

else instance (Reflectable alias String, Reflectable name String) ⇒ ToExpression (Sort (Path alias name)) where
      toExpression s = DLIA.Sorted (qualifiedColumn (Proxy ∷ _ alias) (Proxy ∷ _ name)) (sortDirection s)

-- | e.g., string_agg(field, separator ORDER BY field)
else instance (ToExpressionList f, ToExpression s) ⇒ ToExpression (OrderBy f s) where
      toExpression (OrderBy f s) = DLIA.OrderedBy (toExpression s) (toExpressionList f)

else instance ToExpressionList args ⇒ ToExpression (PgFunction input args fields output) where
      toExpression (PgFunction name args) = DLIA.Function name (toExpressionList args)

else instance (ToExpression inp, ToExpressionList rest) ⇒ ToExpression (Aggregate inp rest fields out) where
      toExpression = case _ of
            Count f → DLIA.Function countFunctionName [ toExpression f ]
            StringAgg f rest → DLIA.Function string_aggFunctionName ([ toExpression f ] <> toExpressionList rest)
            ArrayAgg f → DLIA.Function array_aggFunctionName [ toExpression f ]

else instance ToExpression Default where
      toExpression _ = DLIA.DefaultValue

-- | Parameters
else instance ToValue v ⇒ ToExpression v where
      toExpression v = DLIA.Parameter (DLID.toValue v)

-- | Lower comma separated lists
class ToExpressionList e where
      toExpressionList ∷ e → Array Expression

instance (ToExpressionList e, ToExpressionList f) ⇒ ToExpressionList (Tuple e f) where
      toExpressionList (Tuple e f) = toExpressionList e <> toExpressionList f

-- | Functions without arguments
else instance ToExpressionList Unit where
      toExpressionList _ = []

-- | Aggregates without extra arguments
else instance ToExpressionList E where
      toExpressionList _ = []

else instance ToExpression e ⇒ ToExpressionList e where
      toExpressionList e = [ toExpression e ]

-- | Lower INSERT values: arrays are multiple rows, anything else a single row
class ToRows v where
      toRows ∷ v → Array (Array Expression)

instance ToExpressionList v ⇒ ToRows (Array v) where
      toRows = map toExpressionList

else instance ToExpressionList v ⇒ ToRows v where
      toRows v = [ toExpressionList v ]

-- | Lower UPDATE SET pairs
class ToSetPairs pairs where
      toSetPairs ∷ pairs → Array (Tuple String Expression)

instance (Reflectable name String, ToExpression v) ⇒ ToSetPairs (Op (Proxy name) v) where
      toSetPairs (Op _ _ v) = [ Tuple (DR.reflectType (Proxy ∷ _ name)) (toExpression v) ]

instance (ToSetPairs p, ToSetPairs rest) ⇒ ToSetPairs (Tuple p rest) where
      toSetPairs (Tuple p rest) = toSetPairs p <> toSetPairs rest

-- | Print a lowered statement
fromStatement ∷ ∀ projection. Statement → Query projection
fromStatement statement = Query Nothing query parameters
      where
      { query, parameters } = DLIA.print statement

qualifiedColumn ∷ ∀ alias name. Reflectable alias String ⇒ Reflectable name String ⇒ Proxy alias → Proxy name → Expression
qualifiedColumn alias name = DLIA.QualifiedColumn (DR.reflectType alias) (DR.reflectType name)

sortDirection ∷ ∀ f. Sort f → SortDirection
sortDirection = case _ of
      Asc → DLIA.Ascending
      Desc → DLIA.Descending

-- | CREATE TABLE
instance
      ( RowToList columns columnList
      , TranslateFieldDefinition columnList
      , ConstraintsToRowList columnList list
      , CompositeConstraints list composites
      , TranslateCompositeConstraint composites
      , Reflectable name String
      ) ⇒
      ToStatement (T (Table name columns) Create) where
      toStatement _ = [ DLIA.CreateTable (DR.reflectType (Proxy ∷ _ name)) (translateFieldDefinition (Proxy ∷ _ columnList) <> translateCompositeConstraint (Proxy ∷ _ composites)) ]

-- |
class TranslateFieldDefinition (list ∷ RowList Type) where
      translateFieldDefinition ∷ Proxy list → Array String

instance TranslateFieldDefinition Nil where
      translateFieldDefinition _ = []

instance
      ( Reflectable name String
      , ToType t
      , ToNullableDefinition t
      , ToConstraintDefinition c
      , TranslateFieldDefinition rest
      ) ⇒
      TranslateFieldDefinition (Cons name (Column t c) rest) where
      translateFieldDefinition _ =
            ( quote (Proxy ∷ _ name)
                    <> space
                    <> DLID.toType _t
                    <> toNullableDefinition _t
                    <> toConstraintDefinition (Proxy ∷ _ c)
            )
                  : translateFieldDefinition (Proxy ∷ _ rest)
            where
            _t = Proxy ∷ _ t

else instance (ToType t, ToNullableDefinition t, TranslateFieldDefinition rest, Reflectable name String) ⇒ TranslateFieldDefinition (Cons name t rest) where
      translateFieldDefinition _ = (quote (Proxy ∷ _ name) <> space <> DLID.toType _t <> toNullableDefinition _t) : translateFieldDefinition (Proxy ∷ _ rest)
            where
            _t = Proxy ∷ _ t

-- |
class ToNullableDefinition (t ∷ Type) where
      toNullableDefinition ∷ Proxy t → String

instance ToNullableDefinition (Maybe t) where
      toNullableDefinition _ = ""

else instance ToNullableDefinition t where
      toNullableDefinition _ = notNullKeyword

-- | String representation of constraints
class ToConstraintDefinition (c ∷ Type) where
      toConstraintDefinition ∷ Proxy c → String

instance ToConstraintDefinition Identity where
      toConstraintDefinition _ = identityKeyword

instance ToConstraintDefinition Unique where
      toConstraintDefinition _ = uniqueKeyword

instance ToConstraintDefinition PrimaryKey where
      toConstraintDefinition _ = primaryKeyKeyword

instance (Reflectable tableName String, Reflectable fieldName String) ⇒ ToConstraintDefinition (ForeignKey fieldName (Table tableName f)) where
      toConstraintDefinition _ = referencesKeyword <> quote (Proxy ∷ _ tableName) <> space <> openBracket <> quote (Proxy ∷ _ fieldName) <> closeBracket

--ignore composites as they are special babies
instance ToConstraintDefinition (Constraint (Composite n) t) where
      toConstraintDefinition _ = ""

else instance (Reflectable name String, ToConstraintDefinition t) ⇒ ToConstraintDefinition (Constraint name t) where
      toConstraintDefinition _ = space <> constraintKeyword <> quote (Proxy ∷ _ name) <> toConstraintDefinition (Proxy ∷ _ t)

instance (ToConstraintDefinition some, ToConstraintDefinition more) ⇒ ToConstraintDefinition (Tuple some more) where
      toConstraintDefinition _ = toConstraintDefinition (Proxy ∷ _ some) <> toConstraintDefinition (Proxy ∷ _ more)

-- | Flatten constraints into a list
class CompositeConstraints (source ∷ RowList Type) (constraints ∷ RowList Type) | source → constraints

instance CompositeConstraints Nil Nil

instance
      ( CompositeFields name rest finishing
      , RowListSet starting t finishing fields
      , RowListRemove name rest later
      , CompositeConstraints later tail
      ) ⇒
      CompositeConstraints (Cons name (C starting t) rest) (Cons name (C fields t) tail)

else instance CompositeConstraints rest all ⇒ CompositeConstraints (Cons name r rest) all

-- | Find all fields that are part of a composite constraint
class CompositeFields (name ∷ Symbol) (rest ∷ RowList Type) (fields ∷ RowList Type) | name rest → fields

instance CompositeFields name Nil Nil

instance
      ( CompositeFields name rest tail
      , RowListSet field t tail all
      ) ⇒
      CompositeFields name (Cons name (C field t) rest) all

else instance CompositeFields name rest all ⇒ CompositeFields name (Cons n t rest) all

-- |
class TranslateCompositeConstraint (list ∷ RowList Type) where
      translateCompositeConstraint ∷ Proxy list → Array String

instance TranslateCompositeConstraint Nil where
      translateCompositeConstraint _ = []

instance
      ( Reflectable name String
      , CompositeFieldList fields
      , ToCompositeConstraintDefinition t
      , TranslateCompositeConstraint rest
      , ReferenceList fields constraints
      , ToReferenceDefinition constraints
      ) ⇒
      TranslateCompositeConstraint (Cons name (C fields t) rest) where
      translateCompositeConstraint _ =
            ( constraintKeyword
                    <> quote (Proxy ∷ _ name)
                    <> toCompositeConstraintDefinition (Proxy ∷ _ t)
                    <> openBracket
                    <> DST.joinWith comma (compositeFieldList (Proxy ∷ _ fields))
                    <> closeBracket
                    <> toReferenceDefinition (Proxy ∷ _ constraints)
            )
                  : translateCompositeConstraint (Proxy ∷ _ rest)

-- | While the rest stays the same, CONSTRAINT syntax uses and extra keyword for foreign keys
class ToCompositeConstraintDefinition (t ∷ Type) where
      toCompositeConstraintDefinition ∷ Proxy t → String

instance ToCompositeConstraintDefinition (ForeignKey fn t) where
      toCompositeConstraintDefinition _ = foreignKeyKeyword

else instance ToConstraintDefinition t ⇒ ToCompositeConstraintDefinition t where
      toCompositeConstraintDefinition _ = toConstraintDefinition (Proxy ∷ _ t)

-- | For foreign keys, we need a list of referenced fields
class ReferenceList (source ∷ RowList Type) (constraints ∷ RowList Type) | source → constraints

instance ReferenceList Nil Nil

instance (ReferenceList rest tail, RowListSet name (Composite tableName) tail all) ⇒ ReferenceList (Cons n (ForeignKey name (Table tableName fields)) rest) all

--lists should be of same constraint
else instance ReferenceList (Cons n t rest) Nil

-- |
class ToReferenceDefinition (source ∷ RowList Type) where
      toReferenceDefinition ∷ Proxy source → String

instance ToReferenceDefinition Nil where
      toReferenceDefinition _ = ""

else instance (Reflectable tableName String, CompositeFieldList (Cons fieldName (Composite tableName) rest)) ⇒ ToReferenceDefinition (Cons fieldName (Composite tableName) rest) where
      toReferenceDefinition _ = referencesKeyword <> quote (Proxy ∷ _ tableName) <> space <> openBracket <> DST.joinWith comma (compositeFieldList (Proxy ∷ _ (Cons fieldName (Composite tableName) rest))) <> closeBracket

-- |
class CompositeFieldList (fields ∷ RowList Type) where
      compositeFieldList ∷ Proxy fields → Array String

instance CompositeFieldList Nil where
      compositeFieldList _ = []

instance (CompositeFieldList rest, Reflectable name String) ⇒ CompositeFieldList (Cons name t rest) where
      compositeFieldList _ = quote (Proxy ∷ _ name) : compositeFieldList (Proxy ∷ _ rest)

-- | ALTER TABLE ... ADD
instance
      ( Reflectable tableName String
      , Reflectable name String
      , SingleColumn name column columnList
      , TranslateFieldDefinition columnList
      ) ⇒
      ToStatement (Add name column (T (Table tableName fields) Alter)) where
      toStatement _ = [ DLIA.AlterTableAdd (DR.reflectType (Proxy ∷ _ tableName)) (DM.fromMaybe "" <<< DA.head $ translateFieldDefinition (Proxy ∷ _ columnList)) ]

class SingleColumn (name ∷ Symbol) (c ∷ Type) (output ∷ RowList Type) | name c → output

instance SingleColumn name (Proxy t) (Cons name t Nil)

instance SingleColumn name (Column t constraints) (Cons name (Column t constraints) Nil)

-- | DROP TABLE
instance Reflectable name String ⇒ ToStatement (T (Table name fields) Drop) where
      toStatement _ = [ DLIA.DropTable (DR.reflectType (Proxy ∷ _ name)) ]

quote ∷ ∀ alias. Reflectable alias String ⇒ Proxy alias → String
quote name = quoteSymbol <> DR.reflectType name <> quoteSymbol

buildQuery ∷ ∀ q projection. ToQuery q projection ⇒ q → Query projection
buildQuery = toQuery

unsafeBuildQuery ∷
      ∀ projection parameters parameterList.
      RowToList parameters parameterList ⇒
      ToParameters parameters parameterList ⇒
      Maybe Plan →
      String →
      Record parameters →
      Query projection
unsafeBuildQuery plan q parameters = Query plan dollaredQ parameterValues
      where
      parameterPairs = DLID.toParameters (Proxy ∷ _ parameterList) parameters
      parameterNames = DTP.fst <$> parameterPairs
      parameterValues = DTP.snd <$> parameterPairs
      --HACK
      parameterIndexes = map (\i → parameterSymbol <> show i) (1 .. DA.length parameterNames)
      replace sql (name /\ p) = DSR.replace (DSRU.unsafeRegex (atSymbol <> "\\b" <> name <> "\\b") global) p sql
      dollaredQ = DA.foldl replace q $ DA.zip parameterNames parameterIndexes
