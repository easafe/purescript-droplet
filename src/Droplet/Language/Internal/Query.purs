-- | `ToQuery`, a type class to generate parameterized SQL statement strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Driver` instead
module Droplet.Language.Internal.Query (class FilteredQuery, class QualifiedProjection, class TranslateSource, class ToNakedProjection, class SingleQualifiedColumn, class TranslateConditions, class TranslateColumn, class NoAggregations, class OnlyAggregations, class AggregatedQuery, class IsValidAggregation, class ToJoinType, class ArgumentList, argumentList, class QueryMustNotBeAliased, class ToQuery, toQuery, class TranslateNakedColumn, translateNakedColumn, class NameList, class ToFieldValuePairs, class ToFieldValues, class Translate, Query(..), translateSource, QueryState, translateColumn, toJoinType, nameList, translateConditions, toFieldValuePairs, toFieldValues, translate, buildQuery, unsafeBuildQuery) where

import Control.Monad.State (State)
import Control.Monad.State as CMS
import Data.Array ((..))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DST
import Data.String.Regex as DSR
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe as DSRU
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Traversable as DF
import Data.Traversable as DT
import Data.Tuple (Tuple(..))
import Data.Tuple as DTP
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Language.Internal.Condition (BinaryOperator(..), Exists, In, IsNotNull, Not, Op(..))
import Droplet.Language.Internal.Definition (class AppendPath, class ToParameters, class ToValue, class UnwrapDefinition, class UnwrapNullable, E(..), Path, Star, Table, toParameters, toValue)
import Droplet.Language.Internal.Function (Aggregate(..), PgFunction(..))
import Droplet.Language.Internal.Keyword (allKeyword, andKeyword, asKeyword, ascKeyword, atSymbol, byKeyword, closeBracket, comma, countFunctionName, deleteKeyword, descKeyword, distinctKeyword, dotSymbol, equalsSymbol, existsKeyword, fromKeyword, greaterThanSymbol, groupByKeyword, inKeyword, innerKeyword, insertKeyword, isNotNullKeyword, joinKeyword, leftKeyword, lesserThanSymbol, limitKeyword, notEqualsSymbol, notKeyword, offsetKeyword, onKeyword, openBracket, orKeyword, orderKeyword, parameterSymbol, quoteSymbol, returningKeyword, selectKeyword, setKeyword, starSymbol, string_aggFunctionName, unionKeyword, updateKeyword, valuesKeyword, whereKeyword)
import Droplet.Language.Internal.Syntax (class JoinedToMaybe, class QueryOptionallyAliased, class ToProjection, class ToSingleColumn, class UniqueColumnNames, As(..), Delete(..), Distinct(..), From(..), GroupBy(..), Inclusion(..), Inner, Insert(..), Into(..), Join(..), Limit(..), Offset(..), On(..), OrderBy(..), Outer, Plan, Prepare(..), Returning(..), Select(..), Set(..), Side, Sort(..), Union(..), Update(..), Values(..), Where(..))
import Foreign (Foreign)
import Prelude (class Show, Unit, bind, discard, map, otherwise, pure, show, ($), (<$>), (<>), (==), (||))
import Prim.Boolean (False, True)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, Nil, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Type.Data.Boolean (class And)
import Type.Proxy (Proxy(..))

--this is all really ugly right now

data Query (projection ∷ Row Type) = Query (Maybe Plan) String (Array Foreign)

type QueryState = { plan ∷ Maybe Plan, parameters ∷ Array Foreign, bracketed ∷ Boolean }

instance Show (Query projection) where
      show (Query _ q _) = q

-- | Builds a SQL query
class ToQuery (q ∷ Type) (projection ∷ Row Type) | q → projection where
      toQuery ∷ q → State QueryState String

-- | Fully formed queries in the shape of SELECT ... FROM ...
instance
      ( --aggregation errors
        AggregatedQuery s rest
      , QueryMustNotBeAliased rest
      , --on condition errors
        FilteredQuery f fields
      --condition errors
      , FilteredQuery rest fields
      , --alias errors
        QualifiedProjection s fields qual
      , --qualified columns projection
        Union qual projection all
      , Nub all final
      , Translate (Select s projection (From f fields rest))
      ) ⇒
      ToQuery (Select s projection (From f fields rest)) final where
      toQuery q = translate q

-- | "Naked" queries in the shape of SELECT ...
instance
      ( ToNakedProjection s projection
      , Nub projection unique
      , UniqueColumnNames projection unique
      , Translate (Select s p E)
      ) ⇒
      ToQuery (Select s p E) unique where
      toQuery q = translate q

-- | UNION
instance
      ( ToQuery q final
      , ToQuery r p
      , Translate (Union q r)
      ) ⇒
      ToQuery (Union q r) final where
      toQuery q = translate q

-- | INSERT ... RETURNING
instance
      ( ToProjection f fields Nil projection
      , Translate (Insert (Into name fields fieldNames (Values v (Returning f))))
      ) ⇒
      ToQuery (Insert (Into name fields fieldNames (Values v (Returning f)))) projection where
      toQuery q = translate q

-- | INSERT
instance Translate (Insert (Into name fields fieldNames (Values v E))) ⇒ ToQuery (Insert (Into name fields fieldNames (Values v E))) () where
      toQuery q = translate q

-- | UPDATE
instance Translate (Update table fields (Set values rest)) ⇒ ToQuery (Update table fields (Set values rest)) () where
      toQuery q = translate q

-- | DELETE
instance Translate (Delete (From f fields rest)) ⇒ ToQuery (Delete (From f fields rest)) () where
      toQuery q = translate q

-- | Unsafe queries
instance ToQuery (Query projection) projection where
      toQuery (Query plan q parameters) = do
            CMS.put { plan, parameters, bracketed: false }
            pure q

-- | Asserts that queries not using GROUP BY do not mix aggregated and non aggregated columns
class AggregatedQuery (s ∷ Type) (q ∷ Type)

instance AggregatedQuery s rest ⇒ AggregatedQuery s (Where c rest)

else instance AggregatedQuery s (GroupBy f rest)

else instance AggregatedQuery s q ⇒ AggregatedQuery (Distinct s) q

else instance
      ( NoAggregations s no
      , OnlyAggregations s yes
      , IsValidAggregation no yes
      ) ⇒
      AggregatedQuery s q

-- | Are all columns not aggregated?
class NoAggregations (q ∷ Type) (is ∷ Boolean) | q → is

instance NoAggregations (Aggregate i rest fields o) False

else instance NoAggregations (As n (Aggregate i rest f o)) False

else instance
      ( NoAggregations a isa
      , NoAggregations b isb
      , And isa isb is
      ) ⇒
      NoAggregations (Tuple a b) is

else instance NoAggregations s True

-- | Are all columns aggregated?
class OnlyAggregations (q ∷ Type) (is ∷ Boolean) | q → is

instance OnlyAggregations (Aggregate i rest f o) True

else instance OnlyAggregations (As n (Aggregate i rest f o)) True

else instance
      ( OnlyAggregations a isa
      , OnlyAggregations b isb
      , And isa isb is
      ) ⇒
      OnlyAggregations (Tuple a b) is

else instance OnlyAggregations s False

-- | Check aggregation results
-- |
-- | Having a separated type class leads to better error messages
class IsValidAggregation (s ∷ Boolean) (t ∷ Boolean)

instance Fail (Text "Projection cannot include aggregations. Are you missing a GROUP BY clause?") ⇒ IsValidAggregation False False

else instance IsValidAggregation s t

-- | Prevents top level queries to end in AS
class QueryMustNotBeAliased (q ∷ Type)

instance QueryMustNotBeAliased rest ⇒ QueryMustNotBeAliased (Where c rest)

instance QueryMustNotBeAliased rest ⇒ QueryMustNotBeAliased (GroupBy f rest)

instance QueryMustNotBeAliased rest ⇒ QueryMustNotBeAliased (OrderBy f rest)

instance QueryMustNotBeAliased rest ⇒ QueryMustNotBeAliased (Limit rest)

instance QueryMustNotBeAliased rest ⇒ QueryMustNotBeAliased (Offset rest)

instance Fail (Text "AS statement cannot be top level") ⇒ QueryMustNotBeAliased (As alias E)

instance QueryMustNotBeAliased E

instance QueryMustNotBeAliased (Query projection)

-- | Validates qualified columns that `ToProjection` didn't have the context for
class QualifiedProjection (s ∷ Type) (outer ∷ Row Type) (projection ∷ Row Type) | s → outer projection

instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , JoinedToMaybe t u
      , UnwrapDefinition u v
      , Cons fullPath v () projection
      ) ⇒
      QualifiedProjection (Path alias name) outer projection

else instance
      ( AppendPath table name fullPath
      , Cons fullPath t e outer
      , JoinedToMaybe t u
      , UnwrapDefinition u v
      , Cons alias v () projection
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
      QualifiedProjection (s /\ t) outer projection

else instance
      ( Union outer fields all
      , Nub all nubbed
      , FilteredQuery rest nubbed
      , FilteredQuery f nubbed
      , QualifiedProjection s nubbed projection
      , RowToList projection list
      , SingleQualifiedColumn list rest single
      ) ⇒
      QualifiedProjection (Select s p (From f fields rest)) outer single

else instance QualifiedProjection s outer projection ⇒ QualifiedProjection (Distinct s) outer projection

else instance QualifiedProjection s outer ()

-- | Projects a single qualified column, if it exists
class SingleQualifiedColumn (fields ∷ RowList Type) (q ∷ Type) (single ∷ Row Type) | fields → q single

instance SingleQualifiedColumn RL.Nil q ()

else instance (QueryOptionallyAliased q name alias, Cons alias (Maybe t) () single) ⇒ SingleQualifiedColumn (RL.Cons name (Maybe t) RL.Nil) q single

else instance (QueryOptionallyAliased q name alias, Cons alias (Maybe t) () single) ⇒ SingleQualifiedColumn (RL.Cons name t RL.Nil) q single

-- | would be nice to dry this like tocondition, but gotta find a way to only check `Path`s
-- | Checks for invalid qualified columns usage in conditional clauses
class FilteredQuery (q ∷ Type) (outer ∷ Row Type)

instance FilteredQuery cond outer ⇒ FilteredQuery (Where cond rest) outer

else instance FilteredQuery cond outer ⇒ FilteredQuery (Join k f q r a (On cond rest)) outer

else instance (FilteredQuery (Op a b) outer, FilteredQuery (Op c d) outer) ⇒ FilteredQuery (Op (Op a b) (Op c d)) outer

else instance FilteredQuery (Op a b) outer ⇒ FilteredQuery (Op Not (Op a b)) outer

else instance (AppendPath alias name fullPath, Cons fullPath (Maybe t) e outer) ⇒ FilteredQuery (Op IsNotNull (Path alias name)) outer

else instance
      (
        -- exists support arbitrary queries, so we gotta repeat all of these....
        AggregatedQuery s rest
      , QueryMustNotBeAliased rest
      , QualifiedProjection s outer o
      , Union outer fields os
      , Nub os allOut
      , FilteredQuery rest allOut
      ) ⇒
      FilteredQuery (Op Exists (Select s p (From f fields rest))) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      FilteredQuery (Op In (Op (Path alias name) (Array v))) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapDefinition t v
      , UnwrapNullable v w
      , AppendPath otherAlias otherName otherFullPath
      , Cons otherFullPath u f outer
      , UnwrapDefinition u z
      , UnwrapNullable z w
      ) ⇒
      FilteredQuery (Op (Path alias name) (Path otherAlias otherName)) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapDefinition t v
      , UnwrapNullable v w
      , Cons otherName u f outer
      , UnwrapDefinition u z
      , UnwrapNullable z w
      ) ⇒
      FilteredQuery (Op (Path alias name) (Proxy otherName)) outer

else instance
      ( Cons name t f outer
      , UnwrapDefinition t v
      , UnwrapNullable v w
      , AppendPath alias otherName fullPath
      , Cons fullPath u e outer
      , UnwrapDefinition u z
      , UnwrapNullable z w
      ) ⇒
      FilteredQuery (Op (Proxy name) (Path alias otherName)) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapDefinition t u
      , UnwrapNullable u v
      ) ⇒
      FilteredQuery (Op (Path alias name) v) outer

else instance
      ( AppendPath alias name fullPath
      , Cons fullPath t e outer
      , UnwrapDefinition t u
      , UnwrapNullable u v
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
      ToNakedProjection (s /\ t) projection

else instance
      ( --let ToProjection figure out alias and outer since this is necessarily a subquery
        ToProjection (Select s p (From f fields rest)) () Nil projection
      , QualifiedProjection s fields refs
      , RowToList projection pro
      , ToSingleColumn pro name t
      , Cons name t () single
      ) ⇒
      ToNakedProjection (Select s p (From f fields rest)) single

else instance ToNakedProjection s projection ⇒ ToNakedProjection (Distinct s) projection

else instance ToProjection s () Nil projection ⇒ ToNakedProjection s projection

-- | Print SQL statements
class Translate q where
      translate ∷ q → State QueryState String

-- | End of query
instance Translate E where
      translate _ = pure ""

-- | PREPARE
instance Translate s ⇒ Translate (Prepare s) where
      translate (Prepare s plan) = do
            CMS.modify_ (_ { plan = Just plan })
            translate s

-- | Print naked selects
class TranslateNakedColumn q where
      translateNakedColumn ∷ q → State QueryState String

instance IsSymbol name ⇒ TranslateNakedColumn (As name Int) where
      translateNakedColumn (As n) = pure $ show n <> asKeyword <> quote (Proxy ∷ Proxy name)

instance (IsSymbol name, ArgumentList args) ⇒ TranslateNakedColumn (As name (PgFunction inp args fields out)) where
      translateNakedColumn (As func) = do
            q ← printFunction func
            pure $ q <> asKeyword <> quote (Proxy ∷ Proxy name)

instance (TranslateNakedColumn s, TranslateNakedColumn t) ⇒ TranslateNakedColumn (Tuple s t) where
      translateNakedColumn (Tuple s t) = do
            q ← translateNakedColumn s
            otherQ ← translateNakedColumn t
            pure $ q <> comma <> otherQ

instance Translate (Select s ss (From f fields rest)) ⇒ TranslateNakedColumn (Select s ss (From f fields rest)) where
      translateNakedColumn s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

instance TranslateNakedColumn s ⇒ Translate (Select s pp E) where
      translate (Select s _) = do
            q ← translateNakedColumn s
            pure $ selectKeyword <> q

-- | Fully clothed selects
else instance (TranslateColumn s, Translate rest) ⇒ Translate (Select s projection rest) where
      translate (Select s rest) = do
            --hack
            { bracketed: needsOpenBracket } ← CMS.get
            q ← translateColumn s
            otherQ ← translate rest
            { bracketed: needsCloseBracket } ← CMS.get
            let sel = selectKeyword <> q <> otherQ
            let
                  opened
                        | needsOpenBracket = openBracket <> sel
                        | otherwise = sel
            let
                  closed
                        | needsCloseBracket = opened <> closeBracket
                        | otherwise = opened
            CMS.modify_ $ _ { bracketed = false }
            pure closed

-- | Print selected columns
class TranslateColumn q where
      translateColumn ∷ q → State QueryState String

instance IsSymbol name ⇒ TranslateColumn (Proxy name) where
      translateColumn name = pure $ quote name

else instance
      ( IsSymbol fullPath
      , IsSymbol name
      , IsSymbol alias
      , AppendPath alias name fullPath
      ) ⇒
      TranslateColumn (Path alias name) where
      translateColumn _ = pure $ quotePath (Proxy ∷ Proxy alias) (Proxy ∷ Proxy name) <> " " <> quote (Proxy ∷ Proxy fullPath)

else instance TranslateColumn Star where
      translateColumn _ = pure starSymbol

else instance IsSymbol name ⇒ TranslateColumn (As name Int) where
      translateColumn (As n) = pure $ show n <> asKeyword <> quote (Proxy ∷ Proxy name)

else instance (IsSymbol name, NameList inp, ArgumentList rest) ⇒ TranslateColumn (As name (Aggregate inp rest fields out)) where
      translateColumn (As agg) = do
            q ← printAggregation agg
            pure $ q <> asKeyword <> quote (Proxy ∷ Proxy name)

else instance (IsSymbol name, ArgumentList args) ⇒ TranslateColumn (As name (PgFunction inp args fields out)) where
      translateColumn (As func) = do
            q ← printFunction func
            pure $ q <> asKeyword <> quote (Proxy ∷ Proxy name)

else instance (IsSymbol name, IsSymbol alias) ⇒ TranslateColumn (As alias (Proxy name)) where
      translateColumn _ = pure $ quote (Proxy ∷ Proxy name) <> asKeyword <> quote (Proxy ∷ Proxy alias)

else instance
      ( IsSymbol fullPath
      , IsSymbol alias
      , IsSymbol name
      , IsSymbol table
      , AppendPath table name fullPath
      ) ⇒
      TranslateColumn (As alias (Path table name)) where
      translateColumn _ = pure $ quotePath (Proxy ∷ Proxy table) (Proxy ∷ Proxy name) <> asKeyword <> quote (Proxy ∷ Proxy alias)

else instance (TranslateColumn s, TranslateColumn t) ⇒ TranslateColumn (Tuple s t) where
      translateColumn (s /\ t) = do
            sQ ← translateColumn s
            tQ ← translateColumn t
            pure $ sQ <> comma <> tQ

else instance TranslateColumn s ⇒ TranslateColumn (Distinct s) where
      translateColumn (Distinct s) = do
            q ← translateColumn s
            pure $ distinctKeyword <> q

else instance Translate q ⇒ TranslateColumn q where
      translateColumn s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

-- | FROM
instance (IsSymbol name, Translate rest) ⇒ Translate (From (Table name fields) fields rest) where
      translate (From _ rest) = do
            q ← translate rest
            pure $ fromKeyword <> DS.reflectSymbol (Proxy ∷ Proxy name) <> q

else instance (Translate (Join k fields l r a more), Translate rest) ⇒ Translate (From (Join k fields l r a more) fields rest) where
      translate (From j rest) = do
            q ← translate j
            otherQ ← translate rest
            pure $ fromKeyword <> q <> otherQ

else instance (TranslateSource q, Translate rest) ⇒ Translate (From q fields rest) where
      translate (From s rest) = do
            q ← translateSource s
            otherQ ← translate rest
            pure $ fromKeyword <> q <> otherQ

-- | Print field source
class TranslateSource (q ∷ Type) where
      translateSource ∷ q → State QueryState String

instance Translate (Select s ppp more) ⇒ TranslateSource (Select s ppp more) where
      translateSource s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

instance (IsSymbol name, IsSymbol alias) ⇒ TranslateSource (As alias (Table name fd)) where
      translateSource _ = pure $ DS.reflectSymbol (Proxy ∷ Proxy name) <> asKeyword <> quote (Proxy ∷ Proxy alias)

instance (ToJoinType k, Translate (Join k fields l r a rest)) ⇒ TranslateSource (Join k fields l r a rest) where
      translateSource j = translate j

-- | JOIN
instance
      ( ToJoinType k
      , TranslateSource l
      , TranslateSource r
      , Translate rest
      ) ⇒
      Translate (Join k fields l r a rest) where
      translate (Join l r rest) = do
            left ← translateSource l
            right ← translateSource r
            q ← translate rest
            pure $ left <> toJoinType (Proxy ∷ Proxy k) <> right <> q

-- | Print join type
class ToJoinType (k ∷ Side) where
      toJoinType ∷ Proxy k → String

instance ToJoinType Inner where
      toJoinType _ = innerKeyword <> joinKeyword

instance ToJoinType Outer where
      toJoinType _ = leftKeyword <> joinKeyword

-- | ON
instance (TranslateConditions c, Translate rest) ⇒ Translate (On c rest) where
      translate (On c rest) = do
            q ← translateConditions c
            otherQ ← translate rest
            pure $ onKeyword <> q <> otherQ

-- | (SELECT ... FROM ...) AS
instance IsSymbol name ⇒ Translate (As name E) where
      translate _ = do
            CMS.modify_ (_ { bracketed = false })
            pure $ closeBracket <> asKeyword <> quote (Proxy ∷ Proxy name)

-- | WHERE
instance (TranslateConditions c, Translate rest) ⇒ Translate (Where c rest) where
      translate (Where c rest) = do
            q ← translateConditions c
            otherQ ← translate rest
            pure $ whereKeyword <> q <> otherQ

-- | Print logical conditions
class TranslateConditions c where
      translateConditions ∷ c → State QueryState String

instance Translate (Select s p (From f fd rest)) ⇒ TranslateConditions (Op Exists (Select s p (From f fd rest))) where
      translateConditions (Op _ _ s) = do
            q ← translate s
            pure $ existsKeyword <> openBracket <> q <> closeBracket

else instance TranslateConditions a ⇒ TranslateConditions (Op IsNotNull a) where
      translateConditions (Op _ _ s) = do
            q ← translateConditions s
            pure $ q <> isNotNullKeyword

else instance TranslateConditions a ⇒ TranslateConditions (Op Not a) where
      translateConditions (Op _ _ s) = do
            q ← translateConditions s
            pure $ notKeyword <> q

else instance (TranslateConditions a, TranslateConditions b) ⇒ TranslateConditions (Op In (Op a (Array b))) where
      translateConditions (Op _ _ (Op _ fd values)) = do
            q ← translateConditions fd
            parameters ← DT.traverse translateConditions values
            pure $ q <> inKeyword <> openBracket <> DST.joinWith ", " parameters <> closeBracket

else instance (TranslateConditions a, TranslateConditions b) ⇒ TranslateConditions (Op a b) where
      translateConditions (Op operator a b) = do
            q ← translateConditions a
            otherQ ← translateConditions b
            pure $
                  if operator == Just And || operator == Just Or then
                        openBracket <> q <> printOperator operator <> otherQ <> closeBracket
                  else
                        q <> printOperator operator <> otherQ

else instance IsSymbol name ⇒ TranslateConditions (Proxy name) where
      translateConditions name = pure $ quote name

else instance (IsSymbol alias, IsSymbol name) ⇒ TranslateConditions (Path alias name) where
      translateConditions _ = pure $ quotePath (Proxy ∷ Proxy alias) (Proxy ∷ Proxy name)

else instance ToValue v ⇒ TranslateConditions v where
      translateConditions p = do
            { parameters } ← CMS.modify $ \s@{ parameters } → s { parameters = DA.snoc parameters $ toValue p }
            pure $ "$" <> show (DA.length parameters)

printOperator ∷ Maybe BinaryOperator → String
printOperator = case _ of
      Nothing → ""
      Just op → case op of
            Equals → equalsSymbol
            NotEquals → notEqualsSymbol
            LesserThan → lesserThanSymbol
            GreaterThan → greaterThanSymbol
            And → andKeyword
            Or → orKeyword

-- | GROUP BY
instance (NameList f, Translate rest) ⇒ Translate (GroupBy f rest) where
      translate (GroupBy fields rest) = do
            q ← translate rest
            pure $ groupByKeyword <> nameList fields <> q

-- | UNION
instance (Translate s, Translate r) ⇒ Translate (Union s r) where
      translate (Union inclusion s r) = do
            q ← translate s
            otherQ ← translate r
            pure $ openBracket <> q <> unionKeyword <> printInclusion inclusion <> otherQ <> closeBracket

printInclusion ∷ Inclusion → String
printInclusion = case _ of
      All → allKeyword
      Unique → ""

-- | INSERT
instance
      ( IsSymbol name
      , NameList fieldNames
      , ToFieldValues v
      , Translate rest
      ) ⇒
      Translate (Insert (Into name fields fieldNames (Values v rest))) where
      translate (Insert (Into fieldNames (Values v rest))) = do
            q ← toFieldValues v
            otherQ ← translate rest
            pure $ insertKeyword
                  <> DS.reflectSymbol (Proxy ∷ Proxy name)
                  <> openBracket
                  <> nameList fieldNames
                  <> closeBracket
                  <> valuesKeyword
                  <> openBracket
                  <> q
                  <> closeBracket
                  <>
                        otherQ

-- | Names (possibly) separated by comma
-- |
-- | Used by INSERT, ORDER BY
class NameList fieldNames where
      nameList ∷ fieldNames → String

instance IsSymbol name ⇒ NameList (Proxy name) where
      nameList name = quote name

instance (IsSymbol alias, IsSymbol name) ⇒ NameList (Path alias name) where
      nameList _ = quotePath (Proxy ∷ Proxy alias) (Proxy ∷ Proxy name)

instance NameList Star where
      nameList _ = starSymbol

instance (NameList f, NameList rest) ⇒ NameList (Tuple f rest) where
      nameList (Tuple f rest) = nameList f <> comma <> nameList rest

-- | Name list for functions, or when fields and parameters can be mixed
class ArgumentList v where
      argumentList ∷ v → State QueryState String

instance IsSymbol name ⇒ ArgumentList (Proxy name) where
      argumentList name = pure $ quote name

else instance (IsSymbol alias, IsSymbol name) ⇒ ArgumentList (Path alias name) where
      argumentList _ = pure $ quotePath (Proxy ∷ Proxy alias) (Proxy ∷ Proxy name)

else instance (ArgumentList s, Translate (OrderBy f E)) ⇒ ArgumentList (OrderBy f s) where
      argumentList (OrderBy f s) = do
            q ← translate (OrderBy f E)
            ns ← argumentList s
            pure $ ns <> q

else instance IsSymbol name ⇒ ArgumentList (Sort (Proxy name)) where
      argumentList s = pure $ quote (Proxy ∷ Proxy name) <> case s of
            Desc → descKeyword
            Asc → ascKeyword

else instance (IsSymbol alias, IsSymbol name) ⇒ ArgumentList (Sort (Path alias name)) where
      argumentList s = pure $ quotePath (Proxy ∷ Proxy alias) (Proxy ∷ Proxy name) <> case s of
            Desc → descKeyword
            Asc → ascKeyword

else instance (ArgumentList args) ⇒ ArgumentList (PgFunction input args fields output) where
      argumentList func = printFunction func

--hack for functions that take no arguments
else instance ArgumentList Unit where
      argumentList _ = pure ""

else instance ArgumentList E where
      argumentList _ = pure ""

else instance (ArgumentList f, ArgumentList rest) ⇒ ArgumentList (Tuple f rest) where
      argumentList (Tuple f rest) = do
            af ← argumentList f
            ar ← argumentList rest
            pure $ af <> comma <> ar

else instance ToFieldValues v ⇒ ArgumentList v where
      argumentList v = toFieldValues v

class ToFieldValues fieldValues where
      toFieldValues ∷ fieldValues → State QueryState String

instance (ToFieldValues p, ToFieldValues rest) ⇒ ToFieldValues (Tuple p rest) where
      toFieldValues (Tuple p rest) = do
            q ← toFieldValues p
            otherQ ← toFieldValues rest
            pure $ q <> comma <> otherQ

else instance ToFieldValues u ⇒ ToFieldValues (Array u) where
      toFieldValues values = do
            q ← DF.traverse toFieldValues values
            let sep = closeBracket <> comma <> openBracket --work around Translate Insert adding brackets
            pure $ DST.joinWith sep q

else instance ToValue p ⇒ ToFieldValues p where
      toFieldValues p = do
            { parameters } ← CMS.modify $ \s@{ parameters } → s { parameters = DA.snoc parameters $ toValue p }
            pure $ "$" <> show (DA.length parameters)

-- | UPDATE
instance (IsSymbol name, ToFieldValuePairs pairs, Translate rest) ⇒ Translate (Update name fields (Set pairs rest)) where
      translate (Update (Set pairs rest)) = do
            q ← toFieldValuePairs pairs
            otherQ ← translate rest
            pure $ updateKeyword
                  <> DS.reflectSymbol (Proxy ∷ Proxy name)
                  <> setKeyword
                  <> q
                  <>
                        otherQ

class ToFieldValuePairs pairs where
      toFieldValuePairs ∷ pairs → State QueryState String

instance (IsSymbol name, ToValue p) ⇒ ToFieldValuePairs (Tuple (Proxy name) p) where
      toFieldValuePairs (Tuple name p) = do
            { parameters } ← CMS.modify $ \s@{ parameters } → s { parameters = DA.snoc parameters $ toValue p }
            pure $ DS.reflectSymbol name <> equalsSymbol <> "$" <> show (DA.length parameters)

else instance (ToFieldValuePairs p, ToFieldValuePairs rest) ⇒ ToFieldValuePairs (Tuple p rest) where
      toFieldValuePairs (Tuple p rest) = do
            q ← toFieldValuePairs p
            otherQ ← toFieldValuePairs rest
            pure $ q <> comma <> otherQ

-- | DELETE
instance Translate (From f fields rest) ⇒ Translate (Delete (From f fields rest)) where
      translate (Delete fr) = do
            q ← translate fr
            pure $ deleteKeyword <> q

-- | RETURNING
instance (NameList fieldNames) ⇒ Translate (Returning fieldNames) where
      translate (Returning fieldNames) = pure $ returningKeyword <> nameList fieldNames

-- | ORDER BY
instance (ArgumentList f, Translate rest) ⇒ Translate (OrderBy f rest) where
      translate (OrderBy f rest) = do
            q ← translate rest
            nf ← argumentList f
            pure $ orderKeyword <> byKeyword <> nf <> q

-- | LIMIT
instance Translate rest ⇒ Translate (Limit rest) where
      translate (Limit n rest) = do
            q ← translate rest
            pure $ limitKeyword <> show n <> q

-- | OFFSET
instance Translate rest ⇒ Translate (Offset rest) where
      translate (Offset n rest) = do
            q ← translate rest
            pure $ offsetKeyword <> show n <> q

printAggregation ∷ ∀ inp fields rest out. NameList inp ⇒ ArgumentList rest ⇒ Aggregate inp rest fields out → State QueryState String
printAggregation = case _ of
      Count f → pure $ countFunctionName <> openBracket <> nameList f <> closeBracket
      StringAgg f rest → do
            nrest ← argumentList rest
            pure $ string_aggFunctionName <> openBracket <> nameList f <> comma <> nrest <> closeBracket

printFunction ∷ ∀ inp fields args out. ArgumentList args ⇒ PgFunction inp args fields out → State QueryState String
printFunction (PgFunction name args) = do
      nf ← argumentList args
      pure $ name <> openBracket <> nf <> closeBracket

quote ∷ ∀ alias. IsSymbol alias ⇒ Proxy alias → String
quote name = quoteSymbol <> DS.reflectSymbol name <> quoteSymbol

-- | Columns in the format alias.name must be quoted to avoid problems with ambiguous column names and *
quotePath ∷ ∀ alias name. IsSymbol alias ⇒ IsSymbol name ⇒ Proxy alias → Proxy name → String
quotePath alias name = quote alias <> dotSymbol <> quote name

buildQuery ∷ ∀ q projection. ToQuery q projection ⇒ q → Query projection
buildQuery qr = Query plan q parameters
      where
      Tuple q { plan, parameters } = CMS.runState (toQuery qr)
            { plan: Nothing
            , parameters: []
            , bracketed: false
            }

unsafeBuildQuery ∷
      ∀ projection parameters pra.
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      Maybe Plan →
      String →
      Record parameters →
      Query projection
unsafeBuildQuery plan q p = Query plan dollaredQ parameterValues
      where
      parameterPairs = toParameters (Proxy ∷ Proxy pra) p
      parameterNames = DTP.fst <$> parameterPairs
      parameterValues = DTP.snd <$> parameterPairs
      --HACK
      parameterIndexes = map (\i → parameterSymbol <> show i) (1 .. DA.length parameterNames)
      replace sql (Tuple name p) = DSR.replace (DSRU.unsafeRegex (atSymbol <> "\\b" <> name <> "\\b") global) p sql
      dollaredQ = DA.foldl replace q $ DA.zip parameterNames parameterIndexes
