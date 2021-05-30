-- | `ToQuery`, a type class to generate parameterized SQL statement strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Driver` instead
module Droplet.Language.Internal.Query (class ToColumnQuery, class ToNakedColumnQuery, toNakedColumnQuery, class ToAggregateName, toAggregateName, class ToFieldNames, class ToSortNames, toSortNames, class ToFieldValuePairs, class ToFieldValues, class ToQuery, Query(..), QueryState, toColumnQuery, toFieldNames, toFieldValuePairs, toFieldValues, toQuery, query, unsafeQuery) where

import Droplet.Language.Internal.Condition
import Droplet.Language.Internal.Definition
import Droplet.Language.Internal.Keyword
import Droplet.Language.Internal.Syntax
import Prelude

import Control.Monad.State (State)
import Control.Monad.State as CMS
import Data.Array ((..))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex as DSR
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe as DSRU
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Data.Tuple as DTP
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Function (Aggregate(..))
import Foreign (Foreign)
import Prim.Row (class Nub)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))


data Query (projection :: Row Type) = Query (Maybe Plan) String (Array Foreign)

type QueryState = { plan :: Maybe Plan, parameters :: Array Foreign, bracketed :: Boolean }

instance queryShow :: Show (Query projection) where
      show (Query _ q _) = q

--lets clean up projection
class ToQuery q (projection :: Row Type) | q -> projection where
      toQuery :: q -> State QueryState String

{-

ToQuery should print valid sql strings but reject queries that can't be executed, with the following considerations

1. naked selects (i.e. not projecting from a source) can be potentially invalid (e.g. SELECT id) so only a limited sub set of SELECT is accepted as top level

2. literal values in comparisions are automatically bound to parameters
      $1, $2, $n, in the order they appear, left to right

3. PREPARE statements are not printed here
      pg will do it for us

-}

--trash
instance eToQuery :: ToQuery E () where
      toQuery _ = pure ""

instance queryToQuery :: ToQuery (Query projection) projection where
      toQuery (Query p q parameters) = do
            CMS.put { plan: p, parameters, bracketed: false }
            pure q

--prepare
instance prepareToQuery :: (ToQuery s projection) => ToQuery (Prepare s) projection where
      toQuery (Prepare s plan) = do
            CMS.modify_ (_ { plan = Just plan })
            toQuery s

--naked selects
class ToNakedColumnQuery q where
      toNakedColumnQuery :: q -> State QueryState String

instance intToNakedColumnQuery :: IsSymbol name => ToNakedColumnQuery (As name Int) where
      toNakedColumnQuery (As n) = pure $ show n <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

instance tupleToNakedColumnQuery :: (ToNakedColumnQuery s, ToNakedColumnQuery t) => ToNakedColumnQuery (Tuple s t) where
      toNakedColumnQuery (Tuple s t) = do
            q <- toNakedColumnQuery s
            otherQ <- toNakedColumnQuery t
            pure $ q <> comma <> otherQ

instance selNakedSelectToNakedColumnQuery :: ToQuery (Select s ss (From f fields extra rest)) p => ToNakedColumnQuery (Select s ss (From f fields extra rest)) where
      toNakedColumnQuery s = do
            CMS.modify_ (_ { bracketed = true })
            toQuery s

instance selectToQuery :: (
      ToNakedColumnQuery s,
      ToProjection s () () projection,
      Nub projection unique,
      UniqueColumnNames projection unique
) => ToQuery (Select s pp E) unique where
      toQuery (Select s _) = do
            q <- toNakedColumnQuery s
            pure $ selectKeyword <> q

--fully clothed selects
else instance fullSelectToQuery :: (ToColumnQuery s, ToQuery rest p) => ToQuery (Select s projection rest) projection where
      toQuery (Select s rest) = do
            --hack
            { bracketed: needsOpenBracket } <- CMS.get
            q <- toColumnQuery s
            otherQ <- toQuery rest
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

class ToColumnQuery q where
      toColumnQuery :: q -> State QueryState String

instance fieldToColumnQuery :: IsSymbol name => ToColumnQuery (Proxy name) where
      toColumnQuery name = pure $ DS.reflectSymbol name

else instance dotToColumnQuery :: IsSymbol name => ToColumnQuery (Path name) where
      toColumnQuery name = pure $ ref <> asKeyword <> quoteSymbol <> ref <> quoteSymbol
            where ref = DS.reflectSymbol name

else instance tableToColumnQuery :: ToColumnQuery Star where
      toColumnQuery _ = pure starSymbol

else instance asIntToColumnQuery :: IsSymbol name => ToColumnQuery (As name Int) where
      toColumnQuery (As n) = pure $ show n <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

else instance asAggregateToColumnQuery :: (IsSymbol name, ToAggregateName inp) => ToColumnQuery (As name (Aggregate inp fields out)) where
      toColumnQuery (As agg) = pure $ printAggregation agg <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

else instance asFieldToColumnQuery :: (IsSymbol name, IsSymbol alias) => ToColumnQuery (As alias (Proxy name)) where
      toColumnQuery _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy alias)

else instance asDotToColumnQuery :: (IsSymbol name, IsSymbol alias) => ToColumnQuery (As alias (Path name)) where
      toColumnQuery _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy alias)

else instance tupleToColumnQuery :: (ToColumnQuery s, ToColumnQuery t) => ToColumnQuery (Tuple s t) where
      toColumnQuery (s /\ t) = do
            sQ <- toColumnQuery s
            tQ <- toColumnQuery t
            pure $ sQ <> comma <> tQ

else instance elseToColumnQuery :: ToQuery q projection => ToColumnQuery q where
      toColumnQuery s = do
            CMS.modify_ (_ { bracketed = true })
            toQuery s

--from
--typing s instead of (Select s ppp rest) breaks ps chain instance resolution
instance fromAsToQuery :: (ToQuery (Select s ppp more) p, ToQuery rest pp) => ToQuery (From (Select s ppp more) fields extra rest) projection where
      toQuery (From s rest) = do
            CMS.modify_ (_ { bracketed = true })
            q <- toQuery s
            otherQ <- toQuery rest
            pure $ fromKeyword <>  q <> otherQ

else instance fromTableToQuery :: (IsSymbol name, ToQuery rest p) => ToQuery (From (Table name fields) fields extra rest) projection where
      toQuery (From _ rest) = do
            q <- toQuery rest
            pure $ fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name) <> q

--as
--only when renaming a query
instance asToQuery :: IsSymbol name => ToQuery (As name E) p where
      toQuery _ = do
            --as has to come outside brackets
            { bracketed } <- CMS.get
            let as = asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)
            if bracketed then do
                  CMS.modify_ (_ { bracketed = false })
                  pure $ closeBracket <> as
             else
                  pure as

--where
instance whereToQuery :: ToQuery rest p => ToQuery (Where rest) projection where
      toQuery (Where filtered rest) = do
            q <- printFilters filtered
            otherQ <- toQuery rest
            pure $ whereKeyword <> q <> otherQ

printFilters :: Filtered -> State QueryState String
printFilters filtered = case filtered of
      Operation opFields op -> printFilterParameter opFields op
      And filter otherFilter -> printBracketedFilters andKeyword filter otherFilter
      Or filter otherFilter -> printBracketedFilters orKeyword filter otherFilter

printFilterParameter :: OperationFields -> Operator -> State QueryState String
printFilterParameter (OperationFields field otherField) op = do
      q <- fieldParameters field
      otherQ <- fieldParameters otherField
      pure $ q <> printOperator op <> otherQ

printBracketedFilters :: String -> Filtered -> Filtered -> State QueryState String
printBracketedFilters keyword filter otherFilter = do
      q <- printFilters filter
      otherQ <- printFilters otherFilter
      pure $ openBracket <> q <> keyword <> otherQ <> closeBracket

fieldParameters :: Either Foreign String -> State QueryState String
fieldParameters = case _ of
      Right field -> pure field
      Left p -> do
            { parameters } <- CMS.modify $ \s@{ parameters } -> s { parameters = DA.snoc parameters p }
            pure $ "$" <> show (DA.length parameters)

printOperator :: Operator -> String
printOperator = case _ of
      Equals -> equalsSymbol
      NotEquals -> notEqualsSymbol
      LesserThan -> lesserThanSymbol
      GreaterThan -> greaterThanSymbol

--insert
instance insertToQuery :: (IsSymbol name, ToFieldNames fieldNames, ToFieldValues v, ToQuery rest projection) => ToQuery (Insert (Into name fields fieldNames (Values v rest))) projection where
      toQuery (Insert (Into fieldNames (Values v rest))) = do
            q <- toFieldValues v
            otherQ <- toQuery rest
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

instance fieldToFieldNames :: IsSymbol name => ToFieldNames (Proxy name) where
      toFieldNames name = DS.reflectSymbol name

instance tupleToFieldNames :: (ToFieldNames f, ToFieldNames rest) => ToFieldNames (Tuple f rest) where
      toFieldNames (Tuple f rest) = toFieldNames f <> comma <> toFieldNames rest


class ToFieldValues fieldValues where
      toFieldValues :: fieldValues -> State QueryState String

instance tupleToFieldValues :: (ToFieldValues p, ToFieldValues rest) => ToFieldValues (Tuple p rest) where
      toFieldValues (Tuple p rest) = do
            q <- toFieldValues p
            otherQ <- toFieldValues rest
            pure $ q <> comma <> otherQ

else instance fieldToFieldValues :: ToValue p => ToFieldValues p where
      toFieldValues p = do
            {parameters} <- CMS.modify $ \s@{ parameters } -> s { parameters = DA.snoc parameters $ toValue p }
            pure $ "$" <> show (DA.length parameters)

--update
instance updateToQuery :: (IsSymbol name, ToFieldValuePairs pairs, ToQuery rest p) => ToQuery (Update name fields (Set pairs rest)) () where
      toQuery (Update (Set pairs rest)) = do
            q <- toFieldValuePairs pairs
            otherQ <- toQuery rest
            pure $ updateKeyword <>
                  DS.reflectSymbol (Proxy :: Proxy name) <>
                  setKeyword <>
                  q <>
                  otherQ

class ToFieldValuePairs pairs where
      toFieldValuePairs :: pairs -> State QueryState String

instance fieldToFieldValuePairs :: (IsSymbol name, ToValue p) => ToFieldValuePairs (Tuple (Proxy name) p) where
      toFieldValuePairs (Tuple name p) = do
            { parameters } <- CMS.modify $ \s@{ parameters } -> s { parameters = DA.snoc parameters $ toValue p }
            pure $ DS.reflectSymbol name <> equalsSymbol <> "$" <> show (DA.length parameters)

else instance tupleTupleToFieldValuePairs :: (ToFieldValuePairs p, ToFieldValuePairs rest) => ToFieldValuePairs (Tuple p rest) where
      toFieldValuePairs (Tuple p rest) = do
            q <- toFieldValuePairs p
            otherQ <- toFieldValuePairs rest
            pure $ q <> comma <> otherQ

--delete
instance deleteToQuery :: ToQuery (From f fields extra rest) p => ToQuery (Delete (From f fields extra rest)) () where
      toQuery (Delete fr) = do
            q <- toQuery fr
            pure $ deleteKeyword <> q

--returning
instance returningToQuery :: (ToFieldNames fieldNames, ToProjection fieldNames fields () projection) => ToQuery (Returning fields fieldNames) projection where
      toQuery (Returning fieldNames) = pure $ returningKeyword <> toFieldNames fieldNames

--order by
instance orderByToQuery :: (ToSortNames f, ToQuery rest p) => ToQuery (OrderBy f rest) projection where
      toQuery (OrderBy f rest) = do
            q <- toQuery rest
            pure $ orderKeyword <> byKeyword <> toSortNames f <> q


class ToSortNames fieldNames where
      toSortNames :: fieldNames -> String

instance fieldToSortNames :: IsSymbol name => ToSortNames (Proxy name) where
      toSortNames name = DS.reflectSymbol name

instance sortToSortNames :: IsSymbol name => ToSortNames (Sort name) where
      toSortNames s = DS.reflectSymbol (Proxy :: Proxy name) <> case s of
            Desc -> descKeyword
            Asc -> ascKeyword

instance tupleToSortNames :: (ToSortNames f, ToSortNames rest) => ToSortNames (Tuple f rest) where
      toSortNames (Tuple f rest) = toSortNames f <> comma <> toSortNames rest

--limit
instance limitToQuery :: ToQuery rest p => ToQuery (Limit rest) projection where
      toQuery (Limit n rest) = do
            q <- toQuery rest
            pure $ limitKeyword <> show n <> q


printAggregation :: forall inp fields out. ToAggregateName inp => Aggregate inp fields out -> String
printAggregation = case _ of
      Count f -> countFunctionName <> openBracket <> toAggregateName f <> closeBracket

class ToAggregateName f where
      toAggregateName :: f -> String

instance fieldToAggregateName :: IsSymbol name => ToAggregateName (Proxy name) where
      toAggregateName name = DS.reflectSymbol name

instance starToAggregateName :: ToAggregateName Star where
      toAggregateName _ = starSymbol


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
