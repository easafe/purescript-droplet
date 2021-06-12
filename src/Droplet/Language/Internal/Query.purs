-- | `Translate`, a type class to generate parameterized SQL statement strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Driver` instead
module Droplet.Language.Internal.Query (class IsValidReference, class ToOuterProjection, class TranslateSource, class ToNakedProjection, class WithColumn, class TranslateConditions, class TranslateColumn, class ToJoin, class IsValidTopLevel, class ToQuery, toQuery, class TranslateNakedColumn, translateNakedColumn, class ToAggregateName, toAggregateName, class ToFieldNames, class ToSortNames, toSortNames, class ToFieldValuePairs, class ToFieldValues, class Translate, Query(..), translateSource, QueryState, translateColumn, toJoin, toFieldNames, translateConditions, toFieldValuePairs, toFieldValues, translate, query, unsafeQuery) where

import Droplet.Language.Internal.Condition
import Droplet.Language.Internal.Definition
import Droplet.Language.Internal.Keyword
import Droplet.Language.Internal.Syntax hiding (class ToJoin)
import Prelude

import Control.Monad.State (State)
import Control.Monad.State as CMS
import Data.Array ((..))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String.Regex as DSR
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe as DSRU
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Data.Tuple as DTP
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Language.Internal.Function (Aggregate(..))
import Foreign (Foreign)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

--this is all really ugly right now

data Query (projection :: Row Type) = Query (Maybe Plan) String (Array Foreign)

type QueryState = { plan :: Maybe Plan, parameters :: Array Foreign, bracketed :: Boolean }

instance Show (Query projection) where
      show (Query _ q _) = q


class ToQuery (q :: Type) (projection :: Row Type) | q -> projection where
      toQuery :: q -> State QueryState String

instance (
      IsValidTopLevel rest,
      IsTableAliased f table,
      IsNamedSubQuery rest table alias,
      RowToList fields list,
      ToExtraFields list alias outer,
      IsValidReference rest outer,
      ToOuterProjection s outer refs,
      Union refs projection all,
      Nub all final,
      Translate (Select s projection (From f fields rest))
) => ToQuery (Select s projection (From f fields rest)) final where
      toQuery q = translate q

else instance (
      ToNakedProjection s projection,
      Nub projection unique,
      UniqueColumnNames projection unique,
      Translate (Select s p E)
) => ToQuery (Select s p E) unique where
      toQuery q = translate q

else instance (
      ToProjection f fields Empty projection,
      Translate (Insert (Into name fields fieldNames (Values v (Returning f))))
) => ToQuery (Insert (Into name fields fieldNames (Values v (Returning f)))) projection where
      toQuery q = translate q

else instance ToQuery (Query projection) projection where
      toQuery (Query p q parameters) = do
            CMS.put { plan: p, parameters, bracketed: false }
            pure q

else instance Translate s => ToQuery s () where
      toQuery q = translate q


class IsValidTopLevel (q :: Type)

instance IsValidTopLevel rest => IsValidTopLevel (Where c rest)

instance IsValidTopLevel rest => IsValidTopLevel (OrderBy f rest)

instance IsValidTopLevel rest => IsValidTopLevel (Limit rest)

--can be improved but the gist is that select ... from ... as name translates to (select ... from ... ) as t which is not a valid top level
instance Fail (Text "AS statement cannot be top level") => IsValidTopLevel (As alias E)

instance IsValidTopLevel E

instance IsValidTopLevel (Query projection)


class ToOuterProjection (s :: Type) (outer :: Row Type) (projection :: Row Type) | s -> outer projection

instance (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer, Cons fullPath t () projection) => ToOuterProjection (Path alias name) outer projection

else instance (Append table Dot path, Append path name fullPath, Cons fullPath t e outer, Cons alias t () projection) => ToOuterProjection (As alias (Path table name)) outer projection

else instance (ToOuterProjection s outer some, ToOuterProjection t outer more, Union some more projection) => ToOuterProjection (s /\ t) outer projection

else instance (
      IsTableAliased f table,
      IsNamedSubQuery rest table tableAlias,
      RowToList fields fieldList,
      ToExtraFields fieldList tableAlias inner,
      Union outer inner all,
      Nub all nubbed,
      IsValidReference rest nubbed,
      ToOuterProjection s nubbed projection,
      RowToList projection list,
      WithColumn list rest single
) => ToOuterProjection (Select s p (From f fields rest)) outer single

else instance ToOuterProjection s outer ()


--cant use ToSingleColumn as it is not mandatory for a subquery to contain a Path
class WithColumn (fields :: RowList Type) (q :: Type) (single :: Row Type) | fields -> q single

instance WithColumn RL.Nil q ()

else instance (IsNamedSubQuery q name alias, Cons alias (Maybe t) () single) => WithColumn (RL.Cons name (Maybe t) RL.Nil) q single

else instance (IsNamedSubQuery q name alias, Cons alias (Maybe t) () single) => WithColumn (RL.Cons name t RL.Nil) q single


class IsValidReference (q :: Type) (outer :: Row Type)

instance IsValidReference cond outer => IsValidReference (Where cond rest) outer

else instance (IsValidReference (Op a b) outer, IsValidReference (Op c d) outer) => IsValidReference (Op (Op a b) (Op c d)) outer

else instance (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer, Append otherAlias Dot otherPath, Append otherPath otherName otherFullPath, Cons otherFullPath t f outer) => IsValidReference (Op (Path alias name) (Path otherAlias otherName)) outer

else instance (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer, Cons otherName t f outer) => IsValidReference (Op (Path alias name) (Proxy otherName)) outer

else instance (Cons name t f outer, Append alias Dot path, Append path otherName fullPath, Cons fullPath t e outer) => IsValidReference (Op (Proxy name) (Path alias otherName)) outer

else instance (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer) => IsValidReference (Op (Path alias name) t) outer

else instance (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer) => IsValidReference (Op t (Path alias name)) outer

else instance IsValidReference e outer


class ToNakedProjection (s :: Type) (projection :: Row Type)

instance (ToNakedProjection s some, ToNakedProjection t more, Union some more projection) => ToNakedProjection (s /\ t) projection

else instance (
      ToProjection (Select s p (From f fields rest)) () "" projection,
      IsTableAliased f table,
      RowToList fields list,
      ToExtraFields list table outer,
      ToOuterProjection s outer refs,
      RowToList projection pro,
      ToSingleColumn pro name t,
      Cons name t () single
) => ToNakedProjection (Select s p (From f fields rest)) single

else instance ToProjection s () Empty projection => ToNakedProjection s projection


{-

Translate should print valid sql strings but reject queries that can't be executed, with the following considerations

1. naked selects (i.e. not projecting from a source) can be potentially invalid (e.g. SELECT id) so only a limited sub set of SELECT is accepted as top level

2. literal values in comparisions are automatically bound to parameters
      $1, $2, $n, in the order they appear, left to right

3. PREPARE statements are not printed here
      pg will do it for us

-}
class Translate q where
      translate :: q -> State QueryState String

--trash
instance Translate E where
      translate _ = pure ""


--prepare
instance Translate s => Translate (Prepare s) where
      translate (Prepare s plan) = do
            CMS.modify_ (_ { plan = Just plan })
            translate s


--naked selects
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

--fully clothed selects
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


class TranslateColumn q where
      translateColumn :: q -> State QueryState String

instance IsSymbol name => TranslateColumn (Proxy name) where
      translateColumn name = pure $ DS.reflectSymbol name

else instance (IsSymbol fullPath, IsSymbol name, IsSymbol alias, Append alias Dot path, Append path name fullPath) => TranslateColumn (Path alias name) where
      translateColumn _ = pure $
            quotePath (Proxy :: Proxy alias) (Proxy :: Proxy name) <>
            " " <>
            quoteSymbol <> DS.reflectSymbol (Proxy :: Proxy fullPath) <> quoteSymbol

else instance TranslateColumn Star where
      translateColumn _ = pure starSymbol

else instance IsSymbol name => TranslateColumn (As name Int) where
      translateColumn (As n) = pure $ show n <> asKeyword <> quote (Proxy :: Proxy name)

else instance (IsSymbol name, ToAggregateName inp) => TranslateColumn (As name (Aggregate inp fields out)) where
      translateColumn (As agg) = pure $ printAggregation agg <> asKeyword <> quote (Proxy :: Proxy name)

else instance (IsSymbol name, IsSymbol alias) => TranslateColumn (As alias (Proxy name)) where
      translateColumn _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias)

else instance (IsSymbol fullPath, IsSymbol alias, IsSymbol name, IsSymbol table, Append table Dot path, Append path name fullPath) => TranslateColumn (As alias (Path table name)) where
      translateColumn _ = pure $ quotePath (Proxy :: Proxy table) (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias)

else instance (TranslateColumn s, TranslateColumn t) => TranslateColumn (Tuple s t) where
      translateColumn (s /\ t) = do
            sQ <- translateColumn s
            tQ <- translateColumn t
            pure $ sQ <> comma <> tQ

else instance Translate q => TranslateColumn q where
      translateColumn s = do
            CMS.modify_ (_ { bracketed = true })
            translate s


--from
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

class TranslateSource (q :: Type) where
      translateSource :: q -> State QueryState String

instance Translate (Select s ppp more) => TranslateSource (Select s ppp more) where
      translateSource s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

instance (IsSymbol name, IsSymbol alias) => TranslateSource (As alias (Table name fd)) where
      translateSource _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias)

instance (ToJoin k, Translate (Join k fields l r rest)) => TranslateSource (Join k fields l r rest) where
      translateSource j = translate j

--join
instance (ToJoin k, TranslateSource l,TranslateSource r, Translate rest) => Translate (Join k fields l r rest) where
      translate (Join l r rest) = do
            left <- translateSource l
            right <- translateSource r
            q <- translate rest
            pure $ left <> toJoin (Proxy :: Proxy k) <> right <> q


class ToJoin (k :: Side) where
      toJoin :: Proxy k -> String

instance ToJoin Inner where
      toJoin _ = innerKeyword <> joinKeyword

instance ToJoin Outer where
      toJoin _ = outerKeyword <> joinKeyword

--on
instance (TranslateConditions c, Translate rest) => Translate (On c rest) where
      translate (On c rest) = do
            q <- translateConditions c
            otherQ <- translate rest
            pure $ onKeyword <> q <> otherQ

--as
--only when renaming a query
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

--where
instance (TranslateConditions c, Translate rest) => Translate (Where c rest) where
      translate (Where c rest) = do
            q <- translateConditions c
            otherQ <- translate rest
            pure $ whereKeyword <> q <> otherQ

class TranslateConditions c where
      translateConditions :: c -> State QueryState String

instance (TranslateConditions a, TranslateConditions b) => TranslateConditions (Op a b) where
      translateConditions (Op operator a b) = do
            q <- translateConditions a
            otherQ <- translateConditions b
            pure $
                  if operator == And || operator == Or then
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

printOperator :: Operator -> String
printOperator = case _ of
      Equals -> equalsSymbol
      NotEquals -> notEqualsSymbol
      LesserThan -> lesserThanSymbol
      GreaterThan -> greaterThanSymbol
      And -> andKeyword
      Or -> orKeyword

--insert
instance (IsSymbol name, ToFieldNames fieldNames, ToFieldValues v, Translate rest) => Translate (Insert (Into name fields fieldNames (Values v rest))) where
      translate (Insert (Into fieldNames (Values v rest))) = do
            q <- toFieldValues v
            otherQ <- translate rest
            pure $ insertKeyword <>
                  DS.reflectSymbol (Proxy :: Proxy name) <>
                  openBracket <>
                  toFieldNames fieldNames <>
                  closeBracket <>
                  valuesKeyword <>
                  openBracket <>
                  q <>
                  closeBracket <>
                  otherQ

class ToFieldNames fieldNames where
      toFieldNames :: fieldNames -> String

instance IsSymbol name => ToFieldNames (Proxy name) where
      toFieldNames name = DS.reflectSymbol name

instance (ToFieldNames f, ToFieldNames rest) => ToFieldNames (Tuple f rest) where
      toFieldNames (Tuple f rest) = toFieldNames f <> comma <> toFieldNames rest


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

--update
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

--delete
instance Translate (From f fields rest) => Translate (Delete (From f fields rest)) where
      translate (Delete fr) = do
            q <- translate fr
            pure $ deleteKeyword <> q

--returning
instance (ToFieldNames fieldNames) => Translate (Returning fieldNames) where
      translate (Returning fieldNames) = pure $ returningKeyword <> toFieldNames fieldNames

--order by
instance (ToSortNames f, Translate rest) => Translate (OrderBy f rest) where
      translate (OrderBy f rest) = do
            q <- translate rest
            pure $ orderKeyword <> byKeyword <> toSortNames f <> q


class ToSortNames fieldNames where
      toSortNames :: fieldNames -> String

instance IsSymbol name => ToSortNames (Proxy name) where
      toSortNames name = DS.reflectSymbol name

instance IsSymbol name => ToSortNames (Sort name) where
      toSortNames s = DS.reflectSymbol (Proxy :: Proxy name) <> case s of
            Desc -> descKeyword
            Asc -> ascKeyword

instance (ToSortNames f, ToSortNames rest) => ToSortNames (Tuple f rest) where
      toSortNames (Tuple f rest) = toSortNames f <> comma <> toSortNames rest

--limit
instance Translate rest => Translate (Limit rest) where
      translate (Limit n rest) = do
            q <- translate rest
            pure $ limitKeyword <> show n <> q


printAggregation :: forall inp fields out. ToAggregateName inp => Aggregate inp fields out -> String
printAggregation = case _ of
      Count f -> countFunctionName <> openBracket <> toAggregateName f <> closeBracket

class ToAggregateName f where
      toAggregateName :: f -> String

instance IsSymbol name => ToAggregateName (Proxy name) where
      toAggregateName name = DS.reflectSymbol name

instance ToAggregateName Star where
      toAggregateName _ = starSymbol

quote :: forall alias. IsSymbol alias => Proxy alias -> String
quote name = quoteSymbol <> DS.reflectSymbol name <> quoteSymbol

quotePath :: forall alias name. IsSymbol alias => IsSymbol name => Proxy alias -> Proxy name -> String
quotePath alias name = quote alias <> dotSymbol <> DS.reflectSymbol name

query :: forall q projection. ToQuery q projection => q -> Query projection
query qr = Query plan q parameters
      where Tuple q {plan, parameters} = CMS.runState (toQuery qr) { plan: Nothing, parameters: [], bracketed: false }

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
