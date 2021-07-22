-- | `Translate`, a type class to generate parameterized SQL statement strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Driver` instead
module Droplet.Language.Internal.Query (class FilteredQuery, class QualifiedProjection, class TranslateSource, class ToNakedProjection, class SingleQualifiedColumn, class TranslateConditions, class TranslateColumn, class NoAggregations, class OnlyAggregations, class AggregatedQuery, class IsValidAggregation, class ToJoinType, class QueryMustNotBeAliased, class ToQuery, toQuery, class TranslateNakedColumn, translateNakedColumn,  class NameList, class ToFieldValuePairs, class ToFieldValues, class Translate, Query(..), translateSource, QueryState, translateColumn, toJoinType, nameList, translateConditions, toFieldValuePairs, toFieldValues, translate, query, unsafeQuery) where

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
import Data.Traversable as DT
import Data.Tuple (Tuple(..))
import Data.Tuple as DTP
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Language.Internal.Condition (BinaryOperator(..), Exists, IsNotNull, Not, Op(..))
import Droplet.Language.Internal.Definition (class ToParameters, class ToValue, class UnwrapDefinition, class UnwrapNullable, Empty, Path, Star, Table, toParameters, toValue)
import Droplet.Language.Internal.Function (Aggregate(..))
import Droplet.Language.Internal.Keyword (andKeyword, asKeyword, ascKeyword, atSymbol, byKeyword, closeBracket, comma, countFunctionName, deleteKeyword, descKeyword, distinctKeyword, dotSymbol, equalsSymbol, existsKeyword, fromKeyword, greaterThanSymbol, groupByKeyword, inKeyword, innerKeyword, insertKeyword, isNotNullKeyword, joinKeyword, leftKeyword, lesserThanSymbol, limitKeyword, notEqualsSymbol, notKeyword, onKeyword, openBracket, orKeyword, orderKeyword, parameterSymbol, quoteSymbol, returningKeyword, selectKeyword, setKeyword, starSymbol, updateKeyword, valuesKeyword, whereKeyword)
import Droplet.Language.Internal.Syntax (class AppendPath, class JoinedToMaybe, class QualifiedFields, class QueryOptionallyAliased, class SourceAlias, class ToProjection, class ToSingleColumn, class UniqueColumnNames, As(..), Delete(..), Distinct(..), E, From(..), GroupBy(..), Inner, Insert(..), Into(..), Join(..), Limit(..), On(..), OrderBy(..), Outer, Plan, Prepare(..), Returning(..), Select(..), Set(..), Side, Sort(..), Update(..), Values(..), Where(..))
import Foreign (Foreign)
import Prelude (class Show, bind, discard, map, otherwise, pure, show, ($), (<$>), (<>), (==), (||))
import Prim.Boolean (False, True)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Type.Data.Boolean (class And)
import Type.Proxy (Proxy(..))

--this is all really ugly right now

data Query (projection :: Row Type) = Query (Maybe Plan) String (Array Foreign)

type QueryState = { plan :: Maybe Plan, parameters :: Array Foreign, bracketed :: Boolean }

instance Show (Query projection) where
      show (Query _ q _) = q


-- | Prints a SQL query
class ToQuery (q :: Type) (projection :: Row Type) | q -> projection where
      toQuery :: q -> State QueryState String

-- | Fully formed queries in the shape of SELECT ... FROM ...
instance (
      AggregatedQuery s rest, --aggregation errors
      QueryMustNotBeAliased rest, --alias errors
      SourceAlias f alias,
      RowToList fields list,
      QualifiedFields list alias outer, --From currently does not include fields with their source alias
      FilteredQuery rest outer, --where condition errors
      QualifiedProjection s outer qual, --qualified columns projection
      Union qual projection all,
      Nub all final,
      Translate (Select s projection (From f fields rest))
) => ToQuery (Select s projection (From f fields rest)) final where
      toQuery q = translate q

-- | "Naked" queries in the shape of SELECT ...
else instance (
      ToNakedProjection s projection,
      Nub projection unique,
      UniqueColumnNames projection unique,
      Translate (Select s p E)
) => ToQuery (Select s p E) unique where
      toQuery q = translate q

-- | INSERT
else instance (
      ToProjection f fields Empty projection,
      Translate (Insert (Into name fields fieldNames (Values v (Returning f))))
) => ToQuery (Insert (Into name fields fieldNames (Values v (Returning f)))) projection where
      toQuery q = translate q

-- | Trivial instance for unsafe queries
else instance ToQuery (Query projection) projection where
      toQuery (Query p q parameters) = do
            CMS.put { plan: p, parameters, bracketed: false }
            pure q

-- | Queries that don't output data
else instance Translate s => ToQuery s () where
      toQuery q = translate q


-- | Asserts that queries not using GROUP BY do not mix aggregated and non aggregated columns
class AggregatedQuery (s :: Type) (q :: Type)

instance AggregatedQuery s rest => AggregatedQuery s (Where c rest)

else instance AggregatedQuery s (GroupBy f rest)

else instance AggregatedQuery s q => AggregatedQuery (Distinct s) q

else instance (
      NoAggregations s no,
      OnlyAggregations s yes,
      IsValidAggregation no yes
) => AggregatedQuery s q


-- | Are all columns not aggregated?
class NoAggregations (q :: Type) (is :: Boolean) | q -> is

instance NoAggregations (Aggregate i f o) False

else instance NoAggregations (As n (Aggregate i f o)) False

else instance (
      NoAggregations a isa,
      NoAggregations b isb,
      And isa isb is
) => NoAggregations (Tuple a b) is

else instance NoAggregations s True


-- | Are all columns aggregated?
class OnlyAggregations (q :: Type) (is :: Boolean) | q -> is

instance OnlyAggregations (Aggregate i f o) True

else instance OnlyAggregations (As n (Aggregate i f o)) True

else instance (
      OnlyAggregations a isa,
      OnlyAggregations b isb,
      And isa isb is
) => OnlyAggregations (Tuple a b) is

else instance OnlyAggregations s False


-- | Check aggregation results
-- |
-- | Having a separated type class leads to better error messages
class IsValidAggregation (s :: Boolean) (t :: Boolean)

instance Fail (Text "Projection cannot include aggregations. Are you missing a GROUP BY clause?") => IsValidAggregation False False

else instance IsValidAggregation s t


-- | Prevents top level queries to end in AS
class QueryMustNotBeAliased (q :: Type)

instance QueryMustNotBeAliased rest => QueryMustNotBeAliased (Where c rest)

instance QueryMustNotBeAliased rest => QueryMustNotBeAliased (GroupBy f rest)

instance QueryMustNotBeAliased rest => QueryMustNotBeAliased (OrderBy f rest)

instance QueryMustNotBeAliased rest => QueryMustNotBeAliased (Limit rest)

instance Fail (Text "AS statement cannot be top level") => QueryMustNotBeAliased (As alias E)

instance QueryMustNotBeAliased E

instance QueryMustNotBeAliased (Query projection)


-- | Validates qualified columns that `ToProjection` didn't have the context for
class QualifiedProjection (s :: Type) (outer :: Row Type) (projection :: Row Type) | s -> outer projection

instance (
      AppendPath alias name fullPath,
      Cons fullPath t e outer,
      JoinedToMaybe t u,
      UnwrapDefinition u v,
      Cons fullPath v () projection
) => QualifiedProjection (Path alias name) outer projection

else instance (
      AppendPath table name fullPath,
      Cons fullPath t e outer,
      JoinedToMaybe t u,
      UnwrapDefinition u v,
      Cons alias v () projection
) => QualifiedProjection (As alias (Path table name)) outer projection

else instance (
      QualifiedProjection s outer some,
      QualifiedProjection t outer more,
      Union some more projection
) => QualifiedProjection (s /\ t) outer projection

else instance (
      SourceAlias f table,
      QueryOptionallyAliased rest table tableAlias,
      RowToList fields fieldList,
      QualifiedFields fieldList tableAlias inner,
      Union outer inner all,
      Nub all nubbed,
      FilteredQuery rest nubbed,
      QualifiedProjection s nubbed projection,
      RowToList projection list,
      SingleQualifiedColumn list rest single
) => QualifiedProjection (Select s p (From f fields rest)) outer single

else instance QualifiedProjection s outer projection => QualifiedProjection (Distinct s) outer projection

else instance QualifiedProjection s outer ()


-- | Projects a single qualified column, if it exists
class SingleQualifiedColumn (fields :: RowList Type) (q :: Type) (single :: Row Type) | fields -> q single

instance SingleQualifiedColumn RL.Nil q ()

else instance (QueryOptionallyAliased q name alias, Cons alias (Maybe t) () single) => SingleQualifiedColumn (RL.Cons name (Maybe t) RL.Nil) q single

else instance (QueryOptionallyAliased q name alias, Cons alias (Maybe t) () single) => SingleQualifiedColumn (RL.Cons name t RL.Nil) q single


-- | Checks for invalid qualified columns usage in WHERE clauses
class FilteredQuery (q :: Type) (outer :: Row Type)

instance FilteredQuery cond outer => FilteredQuery (Where cond rest) outer

else instance (FilteredQuery (Op a b) outer, FilteredQuery (Op c d) outer) => FilteredQuery (Op (Op a b) (Op c d)) outer

else instance FilteredQuery (Op a b) outer => FilteredQuery (Op Not (Op a b)) outer

else instance (AppendPath alias name fullPath, Cons fullPath (Maybe t) e outer) => FilteredQuery (Op IsNotNull (Path alias name)) outer

else instance (
      -- exists support arbitrary queries, so we gotta repeat all of these....
      AggregatedQuery s rest,
      QueryMustNotBeAliased rest,
      QualifiedProjection s outer o,
      SourceAlias f alias,
      RowToList fields list,
      QualifiedFields list alias souter,
      Union outer souter os,
      Nub os allOut,
      FilteredQuery rest allOut
) => FilteredQuery (Op Exists (Select s p (From f fields rest))) outer

else instance (
      AppendPath alias name fullPath,
      Cons fullPath t e outer,
      AppendPath otherAlias otherName otherFullPath,
      Cons otherFullPath t f outer
) => FilteredQuery (Op (Path alias name) (Path otherAlias otherName)) outer

else instance (
      AppendPath alias name fullPath,
      Cons fullPath t e outer,
      Cons otherName t f outer
) => FilteredQuery (Op (Path alias name) (Proxy otherName)) outer

else instance (
      Cons name t f outer,
      AppendPath alias otherName fullPath,
      Cons fullPath t e outer
) => FilteredQuery (Op (Proxy name) (Path alias otherName)) outer

else instance (
      AppendPath alias name fullPath,
      Cons fullPath t e outer,
      UnwrapDefinition t u,
      UnwrapNullable u v
) => FilteredQuery (Op (Path alias name) v) outer

else instance (
      AppendPath alias name fullPath,
      Cons fullPath t e outer,
      UnwrapDefinition t u,
      UnwrapNullable u v
) => FilteredQuery (Op v (Path alias name)) outer

else instance FilteredQuery e outer


-- | Naked selects may be composed of subqueries whose projections need to be checked and merged individually
class ToNakedProjection (s :: Type) (projection :: Row Type)

instance (
      ToNakedProjection s some,
      ToNakedProjection t more,
      Union some more projection
) => ToNakedProjection (s /\ t) projection

else instance (
      ToProjection (Select s p (From f fields rest)) () Empty projection, --let ToProjection figure out alias and outer since this is necessarily a subquery
      SourceAlias f table,
      RowToList fields list,
      QualifiedFields list table outer,
      QualifiedProjection s outer refs,
      RowToList projection pro,
      ToSingleColumn pro name t,
      Cons name t () single
) => ToNakedProjection (Select s p (From f fields rest)) single

else instance ToNakedProjection s projection => ToNakedProjection (Distinct s) projection

else instance ToProjection s () Empty projection => ToNakedProjection s projection


-- | Print SQL statements
class Translate q where
      translate :: q -> State QueryState String

-- | End of query
instance Translate E where
      translate _ = pure ""


-- | PREPARE
instance Translate s => Translate (Prepare s) where
      translate (Prepare s plan) = do
            CMS.modify_ (_ { plan = Just plan })
            translate s


-- | Print naked selects
class TranslateNakedColumn q where
      translateNakedColumn :: q -> State QueryState String

instance IsSymbol name => TranslateNakedColumn (As name Int) where
      translateNakedColumn (As n) = pure $ show n <> asKeyword <> quote (Proxy :: Proxy name)

instance (TranslateNakedColumn s, TranslateNakedColumn t) => TranslateNakedColumn (Tuple s t) where
      translateNakedColumn (Tuple s t) = do
            q <- translateNakedColumn s
            otherQ <- translateNakedColumn t
            pure $ q <> comma <> otherQ

instance Translate (Select s ss (From f fields rest)) => TranslateNakedColumn (Select s ss (From f fields rest)) where
      translateNakedColumn s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

instance TranslateNakedColumn s => Translate (Select s pp E) where
      translate (Select s _) = do
            q <- translateNakedColumn s
            pure $ selectKeyword <> q


-- | Fully clothed selects
else instance (TranslateColumn s, Translate rest) => Translate (Select s projection rest) where
      translate (Select s rest) = do
            --hack
            { bracketed: needsOpenBracket } <- CMS.get
            q <- translateColumn s
            otherQ <- translate rest
            { bracketed: needsCloseBracket } <- CMS.get
            let sel = selectKeyword <> q <> otherQ
            let opened
                  | needsOpenBracket = openBracket <> sel
                  | otherwise = sel
            let closed
                  | needsCloseBracket = opened <> closeBracket
                  | otherwise = opened
            CMS.modify_ $ _ { bracketed = false }
            pure closed


-- | Print selected columns
class TranslateColumn q where
      translateColumn :: q -> State QueryState String

instance IsSymbol name => TranslateColumn (Proxy name) where
      translateColumn name = pure $ DS.reflectSymbol name

else instance (
      IsSymbol fullPath,
      IsSymbol name,
      IsSymbol alias,
      AppendPath alias name fullPath
) => TranslateColumn (Path alias name) where
      translateColumn _ = pure $
            quotePath (Proxy :: Proxy alias) (Proxy :: Proxy name) <>
            " " <>
            quoteSymbol <> DS.reflectSymbol (Proxy :: Proxy fullPath) <> quoteSymbol

else instance TranslateColumn Star where
      translateColumn _ = pure starSymbol

else instance IsSymbol name => TranslateColumn (As name Int) where
      translateColumn (As n) = pure $ show n <> asKeyword <> quote (Proxy :: Proxy name)

else instance (IsSymbol name, NameList inp) => TranslateColumn (As name (Aggregate inp fields out)) where
      translateColumn (As agg) = pure $ printAggregation agg <> asKeyword <> quote (Proxy :: Proxy name)

else instance (IsSymbol name, IsSymbol alias) => TranslateColumn (As alias (Proxy name)) where
      translateColumn _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias)

else instance (
      IsSymbol fullPath,
      IsSymbol alias,
      IsSymbol name,
      IsSymbol table,
      AppendPath table name fullPath
) => TranslateColumn (As alias (Path table name)) where
      translateColumn _ = pure $ quotePath (Proxy :: Proxy table) (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias)

else instance (TranslateColumn s, TranslateColumn t) => TranslateColumn (Tuple s t) where
      translateColumn (s /\ t) = do
            sQ <- translateColumn s
            tQ <- translateColumn t
            pure $ sQ <> comma <> tQ

else instance TranslateColumn s => TranslateColumn (Distinct s) where
      translateColumn (Distinct s) = do
            q <- translateColumn s
            pure $ distinctKeyword <> q

else instance Translate q => TranslateColumn q where
      translateColumn s = do
            CMS.modify_ (_ { bracketed = true })
            translate s


-- | FROM
instance (IsSymbol name, Translate rest) => Translate (From (Table name fields) fields rest) where
      translate (From _ rest) = do
            q <- translate rest
            pure $ fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name) <> q

else instance (Translate (Join k fields l r more), Translate rest) => Translate (From (Join k fields l r more) fields rest) where
      translate (From j rest) = do
            q <- translate j
            otherQ <- translate rest
            pure $ fromKeyword <> q <> otherQ

else instance (TranslateSource q, Translate rest) => Translate (From q fields rest) where
      translate (From s rest) = do
            q <- translateSource s
            otherQ <- translate rest
            pure $ fromKeyword <> q <> otherQ


-- | Print field source
class TranslateSource (q :: Type) where
      translateSource :: q -> State QueryState String

instance Translate (Select s ppp more) => TranslateSource (Select s ppp more) where
      translateSource s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

instance (IsSymbol name, IsSymbol alias) => TranslateSource (As alias (Table name fd)) where
      translateSource _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias)

instance (ToJoinType k, Translate (Join k fields l r rest)) => TranslateSource (Join k fields l r rest) where
      translateSource j = translate j


-- | JOIN
instance (
      ToJoinType k,
      TranslateSource l,
      TranslateSource r,
      Translate rest
) => Translate (Join k fields l r rest) where
      translate (Join l r rest) = do
            left <- translateSource l
            right <- translateSource r
            q <- translate rest
            pure $ left <> toJoinType (Proxy :: Proxy k) <> right <> q


-- | Print join type
class ToJoinType (k :: Side) where
      toJoinType :: Proxy k -> String

instance ToJoinType Inner where
      toJoinType _ = innerKeyword <> joinKeyword

instance ToJoinType Outer where
      toJoinType _ = leftKeyword <> joinKeyword


-- | ON
instance (TranslateConditions c, Translate rest) => Translate (On c rest) where
      translate (On c rest) = do
            q <- translateConditions c
            otherQ <- translate rest
            pure $ onKeyword <> q <> otherQ


-- | (SELECT ... FROM ...) AS
instance IsSymbol name => Translate (As name E) where
      translate _ = do
            --as has to come outside brackets
            { bracketed } <- CMS.get
            let as = asKeyword <> quote (Proxy :: Proxy name)
            if bracketed then do
                  CMS.modify_ (_ { bracketed = false })
                  pure $ closeBracket <> as
             else
                  pure as


-- | WHERE
instance (TranslateConditions c, Translate rest) => Translate (Where c rest) where
      translate (Where c rest) = do
            q <- translateConditions c
            otherQ <- translate rest
            pure $ whereKeyword <> q <> otherQ


-- | Print logical conditions
class TranslateConditions c where
      translateConditions :: c -> State QueryState String

instance Translate (Select s p (From f fd rest)) => TranslateConditions (Op Exists (Select s p (From f fd rest))) where
      translateConditions (Op _ _ s) = do
            q <- translate s
            pure $ existsKeyword <> openBracket <> q <> closeBracket

else instance TranslateConditions a => TranslateConditions (Op IsNotNull a) where
      translateConditions (Op _ _ s) = do
            q <- translateConditions s
            pure $ q <> isNotNullKeyword

else instance TranslateConditions a => TranslateConditions (Op Not a) where
      translateConditions (Op _ _ s) = do
            q <- translateConditions s
            pure $ notKeyword <> q

else instance(TranslateConditions a, TranslateConditions b) => TranslateConditions (Op a (Array b)) where
      translateConditions (Op e fd values) = do
            q <- translateConditions fd
            parameters <- DT.traverse translateConditions values
            pure $ q <> printOperator e <> openBracket <> DST.joinWith ", " parameters <> closeBracket

else instance (TranslateConditions a, TranslateConditions b) => TranslateConditions (Op a b) where
      translateConditions (Op operator a b) = do
            q <- translateConditions a
            otherQ <- translateConditions b
            pure $
                  if operator == Just And || operator == Just Or then
                        openBracket <> q <> printOperator operator <> otherQ <> closeBracket
                   else
                        q <> printOperator operator <> otherQ

else instance IsSymbol name => TranslateConditions (Proxy name) where
      translateConditions name = pure $ DS.reflectSymbol name

else instance (IsSymbol alias, IsSymbol name) => TranslateConditions (Path alias name) where
      translateConditions _ = pure $ quotePath (Proxy :: Proxy alias) (Proxy :: Proxy name)

else instance ToValue v => TranslateConditions v where
      translateConditions p = do
            { parameters } <- CMS.modify $ \s@{ parameters } -> s { parameters = DA.snoc parameters $ toValue p }
            pure $ "$" <> show (DA.length parameters)

printOperator :: Maybe BinaryOperator -> String
printOperator = case _ of
      Nothing -> ""
      Just op -> case op of
            Equals -> equalsSymbol
            NotEquals -> notEqualsSymbol
            LesserThan -> lesserThanSymbol
            GreaterThan -> greaterThanSymbol
            And -> andKeyword
            Or -> orKeyword
            In -> inKeyword


-- | GROUP BY
instance (NameList f, Translate rest) => Translate (GroupBy f rest) where
      translate (GroupBy fields rest) = do
            q <- translate rest
            pure $ groupByKeyword <> nameList fields <> q


-- | INSERT
instance (
      IsSymbol name,
      NameList fieldNames,
      ToFieldValues v,
      Translate rest
) => Translate (Insert (Into name fields fieldNames (Values v rest))) where
      translate (Insert (Into fieldNames (Values v rest))) = do
            q <- toFieldValues v
            otherQ <- translate rest
            pure $ insertKeyword <>
                  DS.reflectSymbol (Proxy :: Proxy name) <>
                  openBracket <>
                  nameList fieldNames <>
                  closeBracket <>
                  valuesKeyword <>
                  openBracket <>
                  q <>
                  closeBracket <>
                  otherQ


-- | Names (possibly) separated by comma
-- |
-- | Used by INSERT, ORDER BY and aggregate functions
class NameList fieldNames where
      nameList :: fieldNames -> String

instance IsSymbol name => NameList (Proxy name) where
      nameList name = DS.reflectSymbol name

instance IsSymbol name => NameList (Sort (Proxy name)) where
      nameList s = DS.reflectSymbol (Proxy :: Proxy name) <> case s of
            Desc -> descKeyword
            Asc -> ascKeyword

instance (IsSymbol alias, IsSymbol name) => NameList (Sort (Path alias name)) where
      nameList s = quotePath (Proxy :: Proxy alias) (Proxy :: Proxy name) <> case s of
            Desc -> descKeyword
            Asc -> ascKeyword

instance (IsSymbol alias, IsSymbol name) => NameList (Path alias name) where
      nameList _ =  quote (Proxy :: Proxy alias) <> dotSymbol <> DS.reflectSymbol (Proxy :: Proxy name)

instance NameList Star where
      nameList _ = starSymbol

instance (NameList f, NameList rest) => NameList (Tuple f rest) where
      nameList (Tuple f rest) = nameList f <> comma <> nameList rest


class ToFieldValues fieldValues where
      toFieldValues :: fieldValues -> State QueryState String

instance (ToFieldValues p, ToFieldValues rest) => ToFieldValues (Tuple p rest) where
      toFieldValues (Tuple p rest) = do
            q <- toFieldValues p
            otherQ <- toFieldValues rest
            pure $ q <> comma <> otherQ

else instance ToValue p => ToFieldValues p where
      toFieldValues p = do
            {parameters} <- CMS.modify $ \s@{ parameters } -> s { parameters = DA.snoc parameters $ toValue p }
            pure $ "$" <> show (DA.length parameters)


-- | UPDATE
instance (IsSymbol name, ToFieldValuePairs pairs, Translate rest) => Translate (Update name fields (Set pairs rest)) where
      translate (Update (Set pairs rest)) = do
            q <- toFieldValuePairs pairs
            otherQ <- translate rest
            pure $ updateKeyword <>
                  DS.reflectSymbol (Proxy :: Proxy name) <>
                  setKeyword <>
                  q <>
                  otherQ


class ToFieldValuePairs pairs where
      toFieldValuePairs :: pairs -> State QueryState String

instance (IsSymbol name, ToValue p) => ToFieldValuePairs (Tuple (Proxy name) p) where
      toFieldValuePairs (Tuple name p) = do
            { parameters } <- CMS.modify $ \s@{ parameters } -> s { parameters = DA.snoc parameters $ toValue p }
            pure $ DS.reflectSymbol name <> equalsSymbol <> "$" <> show (DA.length parameters)

else instance (ToFieldValuePairs p, ToFieldValuePairs rest) => ToFieldValuePairs (Tuple p rest) where
      toFieldValuePairs (Tuple p rest) = do
            q <- toFieldValuePairs p
            otherQ <- toFieldValuePairs rest
            pure $ q <> comma <> otherQ


-- | DELETE
instance Translate (From f fields rest) => Translate (Delete (From f fields rest)) where
      translate (Delete fr) = do
            q <- translate fr
            pure $ deleteKeyword <> q


-- | RETURNING
instance (NameList fieldNames) => Translate (Returning fieldNames) where
      translate (Returning fieldNames) = pure $ returningKeyword <> nameList fieldNames


-- | ORDER BY
instance (NameList f, Translate rest) => Translate (OrderBy f rest) where
      translate (OrderBy f rest) = do
            q <- translate rest
            pure $ orderKeyword <> byKeyword <> nameList f <> q


-- | LIMIT
instance Translate rest => Translate (Limit rest) where
      translate (Limit n rest) = do
            q <- translate rest
            pure $ limitKeyword <> show n <> q


printAggregation :: forall inp fields out. NameList inp => Aggregate inp fields out -> String
printAggregation = case _ of
      Count f -> countFunctionName <> openBracket <> nameList f <> closeBracket

quote :: forall alias. IsSymbol alias => Proxy alias -> String
quote name = quoteSymbol <> DS.reflectSymbol name <> quoteSymbol

quotePath :: forall alias name. IsSymbol alias => IsSymbol name => Proxy alias -> Proxy name -> String
quotePath alias name = quote alias <> dotSymbol <> DS.reflectSymbol name

query :: forall q projection. ToQuery q projection => q -> Query projection
query qr = Query plan q parameters
      where Tuple q {plan, parameters} = CMS.runState (toQuery qr) {
                  plan: Nothing,
                  parameters: [],
                  bracketed: false
            }

unsafeQuery :: forall projection parameters pra.
      RowToList parameters pra =>
      ToParameters parameters pra =>
      Maybe Plan ->
      String ->
      Record parameters ->
      Query projection
unsafeQuery plan q p = Query plan dollaredQ parameterValues
      where parameterPairs = toParameters (Proxy :: Proxy pra) p
            parameterNames = DTP.fst <$> parameterPairs
            parameterValues = DTP.snd <$> parameterPairs
            --HACK
            parameterIndexes = map (\i -> parameterSymbol <> show i) (1 .. DA.length parameterNames)
            replace sql (Tuple name p) = DSR.replace (DSRU.unsafeRegex (atSymbol <> "\\b" <> name <> "\\b") global) p sql
            dollaredQ = DA.foldl replace q $ DA.zip parameterNames parameterIndexes
