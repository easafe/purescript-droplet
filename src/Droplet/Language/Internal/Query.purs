-- | `Translate`, a type class to generate parameterized SQL statement strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Driver` instead
module Droplet.Language.Internal.Query (class IsValidReference, class ToOuterProjection, class ToNakedProjection, class WithColumn, class ToWhereFields, class TranslateColumn, class IsValidTopLevel, class ToQuery, toQuery, class TranslateNakedColumn, translateNakedColumn, class ToAggregateName, class ToExtraFields, toAggregateName, class ToFieldNames, class ToSortNames, toSortNames, class ToFieldValuePairs, class ToFieldValues, class Translate, Query(..), QueryState, translateColumn, toFieldNames, toWhereFields, toFieldValuePairs, toFieldValues, translate, query, unsafeQuery) where

import Droplet.Language.Internal.Condition
import Droplet.Language.Internal.Definition
import Droplet.Language.Internal.Keyword
import Droplet.Language.Internal.Syntax
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
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

--this is all really ugly right now

data Query (projection :: Row Type) = Query (Maybe Plan) String (Array Foreign)

type QueryState = { plan :: Maybe Plan, parameters :: Array Foreign, bracketed :: Boolean }

instance queryShow :: Show (Query projection) where
      show (Query _ q _) = q


class ToQuery (q :: Type) (projection :: Row Type) | q -> projection where
      toQuery :: q -> State QueryState String

instance selToQuery :: (
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

else instance nselToQuery :: (
      ToNakedProjection s projection,
      Nub projection unique,
      UniqueColumnNames projection unique,
      Translate (Select s p E)
) => ToQuery (Select s p E) unique where
      toQuery q = translate q

else instance returningToQuery :: (ToProjection f fields Empty projection, Translate (Insert (Into name fields fieldNames (Values v (Returning fields f))))) => ToQuery (Insert (Into name fields fieldNames (Values v (Returning fields f)))) projection where
      toQuery q = translate q

else instance queryToQuery :: ToQuery (Query projection) projection where
      toQuery (Query p q parameters) = do
            CMS.put { plan: p, parameters, bracketed: false }
            pure q

else instance elseToQuery :: Translate s => ToQuery s () where
      toQuery q = translate q


class IsValidTopLevel (q :: Type)

instance whereIsValidTopLevel :: IsValidTopLevel rest => IsValidTopLevel (Where c rest)

instance orderByIsValidTopLevel :: IsValidTopLevel rest => IsValidTopLevel (OrderBy f rest)

instance limitIsValidTopLevel :: IsValidTopLevel rest => IsValidTopLevel (Limit rest)

--can be improved but the gist is that select ... from ... as name translates to (select ... from ... ) as t which is not a valid top level
instance asIsValidTopLevel :: Fail (Text "AS statement cannot be top level") => IsValidTopLevel (As alias E)

instance eIsValidTopLevel :: IsValidTopLevel E

instance qIsValidTopLevel :: IsValidTopLevel (Query projection)


class ToOuterProjection (s :: Type) (outer :: Row Type) (projection :: Row Type) | s -> outer projection

instance pathToProjection :: (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer, Cons fullPath t () projection) => ToOuterProjection (Path alias name) outer projection

else instance pathAAsToProjection :: (Append table Dot path, Append path name fullPath, Cons fullPath t e outer, Cons alias t () projection) => ToOuterProjection (As alias (Path table name)) outer projection

else instance tupleToProjection :: (ToOuterProjection s outer some, ToOuterProjection t outer more, Union some more projection) => ToOuterProjection (s /\ t) outer projection

else instance selectFromRestToProjection :: (
      IsTableAliased f table,
      IsNamedSubQuery rest table tableAlias,
      RowToList fields fieldList,
      ToExtraFields fieldList tableAlias inner,
      Union outer inner all,
      IsValidReference rest all,
      ToOuterProjection s all projection,
      RowToList projection list,
      WithColumn list rest single
) => ToOuterProjection (Select s p (From f fields rest)) outer single

else instance elseToProjection :: ToOuterProjection s outer ()


--cant use ToSingleColumn as it is not mandatory for a subquery to contain a Path
class WithColumn (fields :: RowList Type) (q :: Type) (single :: Row Type) | fields -> q single

instance nilToSingleColumn :: WithColumn RL.Nil q ()

else instance singleToSingleColumn :: (IsNamedSubQuery q name alias, Cons alias (Maybe t) () single) => WithColumn (RL.Cons name (Maybe t) RL.Nil) q single

else instance singleMaybeToSingleColumn :: (IsNamedSubQuery q name alias, Cons alias (Maybe t) () single) => WithColumn (RL.Cons name t RL.Nil) q single


class ToExtraFields (list :: RowList Type) (alias :: Symbol) (extra :: Row Type) | list alias -> extra

instance nilToExtraFields :: ToExtraFields RL.Nil alias ()

instance consToExtraFields :: (
      Append alias Dot path,
      Append path name fullPath,
      UnwrapDefinition t u,
      Cons fullPath u () head,
      ToExtraFields rest alias tail,
      Lacks fullPath tail,
      Union head tail all
) => ToExtraFields (RL.Cons name t rest) alias all


class IsValidReference (q :: Type) (outer :: Row Type)

instance whereIsValidReference :: IsValidReference cond outer => IsValidReference (Where cond rest) outer

else instance where2IsValidReference :: (IsValidReference (Op a b) outer, IsValidReference (Op c d) outer) => IsValidReference (Op (Op a b) (Op c d)) outer

else instance where3IsValidReference :: (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer, Append otherAlias Dot path, Append path otherName otherFullPath, Cons otherFullPath t f outer) => IsValidReference (Op (Path alias name) (Path otherAlias otherName)) outer

else instance where4IsValidReference :: (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer, Cons otherFullPath t f outer) => IsValidReference (Op (Path alias name) (Proxy otherName)) outer

else instance where5IsValidReference :: (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer, Cons otherFullPath t f outer) => IsValidReference (Op (Proxy otherName) (Path alias name)) outer

else instance where6IsValidReference :: (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer) => IsValidReference (Op (Path alias name) t) outer

else instance where7IsValidReference :: (Append alias Dot path, Append path name fullPath, Cons fullPath t e outer) => IsValidReference (Op t (Path alias name)) outer

else instance eIsValidReference :: IsValidReference e outer

class ToNakedProjection (s :: Type) (projection :: Row Type)

instance tupleToNProjection :: (ToNakedProjection s some, ToNakedProjection t more, Union some more projection) => ToNakedProjection (s /\ t) projection

else instance selToNProjection :: (
      ToProjection (Select s p (From f fields rest)) () "" projection,
      IsTableAliased f table,
      RowToList fields list,
      ToExtraFields list table outer,
      ToOuterProjection s outer refs,
      RowToList projection pro,
      ToSingleColumn pro name t,
      Cons name t () single
) => ToNakedProjection (Select s p (From f fields rest)) single

else instance elseToNProjection :: ToProjection s () Empty projection => ToNakedProjection s projection



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
instance eTranslate :: Translate E where
      translate _ = pure ""


--prepare
instance prepareTranslate :: Translate s => Translate (Prepare s) where
      translate (Prepare s plan) = do
            CMS.modify_ (_ { plan = Just plan })
            translate s

--naked selects
class TranslateNakedColumn q where
      translateNakedColumn :: q -> State QueryState String

instance intTranslateNakedColumn :: IsSymbol name => TranslateNakedColumn (As name Int) where
      translateNakedColumn (As n) = pure $ show n <> asKeyword <> quote (Proxy :: Proxy name)

instance tupleTranslateNakedColumn :: (TranslateNakedColumn s, TranslateNakedColumn t) => TranslateNakedColumn (Tuple s t) where
      translateNakedColumn (Tuple s t) = do
            q <- translateNakedColumn s
            otherQ <- translateNakedColumn t
            pure $ q <> comma <> otherQ

instance selNakedSelectTranslateNakedColumn :: Translate (Select s ss (From f fields rest)) => TranslateNakedColumn (Select s ss (From f fields rest)) where
      translateNakedColumn s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

instance selectTranslate :: TranslateNakedColumn s => Translate (Select s pp E) where
      translate (Select s _) = do
            q <- translateNakedColumn s
            pure $ selectKeyword <> q

--fully clothed selects
else instance fullSelectTranslate :: (TranslateColumn s, Translate rest) => Translate (Select s projection rest) where
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

instance fieldTranslateColumn :: IsSymbol name => TranslateColumn (Proxy name) where
      translateColumn name = pure $ DS.reflectSymbol name

else instance pathTranslateColumn :: (IsSymbol fullPath, Append table Dot path, Append path name fullPath) => TranslateColumn (Path table name) where
      translateColumn _ = pure $ quoteColumn (Proxy :: Proxy fullPath)

else instance tableTranslateColumn :: TranslateColumn Star where
      translateColumn _ = pure starSymbol

else instance asIntTranslateColumn :: IsSymbol name => TranslateColumn (As name Int) where
      translateColumn (As n) = pure $ show n <> asKeyword <> quote (Proxy :: Proxy name)

else instance asAggregateTranslateColumn :: (IsSymbol name, ToAggregateName inp) => TranslateColumn (As name (Aggregate inp fields out)) where
      translateColumn (As agg) = pure $ printAggregation agg <> asKeyword <> quote (Proxy :: Proxy name)

else instance asFieldTranslateColumn :: (IsSymbol name, IsSymbol alias) => TranslateColumn (As alias (Proxy name)) where
      translateColumn _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias)

else instance asPathTranslateColumn :: (IsSymbol fullPath, IsSymbol alias, Append table Dot path, Append path name fullPath) => TranslateColumn (As alias (Path table name)) where
      translateColumn _ = pure $ DS.reflectSymbol (Proxy :: Proxy fullPath) <> asKeyword <> quote (Proxy :: Proxy alias)

else instance tupleTranslateColumn :: (TranslateColumn s, TranslateColumn t) => TranslateColumn (Tuple s t) where
      translateColumn (s /\ t) = do
            sQ <- translateColumn s
            tQ <- translateColumn t
            pure $ sQ <> comma <> tQ

else instance elseTranslateColumn :: Translate q => TranslateColumn q where
      translateColumn s = do
            CMS.modify_ (_ { bracketed = true })
            translate s

--from
--typing s instead of (Select s ppp rest) breaks ps chain instance resolution
instance fromAsTranslate :: (Translate (Select s ppp more), Translate rest) => Translate (From (Select s ppp more) fields rest) where
      translate (From s rest) = do
            CMS.modify_ (_ { bracketed = true })
            q <- translate s
            otherQ <- translate rest
            pure $ fromKeyword <> q <> otherQ

else instance fromAsTableTranslate :: (IsSymbol name, IsSymbol alias, Translate rest) => Translate (From (As alias (Table name fd)) fields rest) where
      translate (From _ rest) = do
            q <- translate rest
            pure $ fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> quote (Proxy :: Proxy alias) <> q

else instance fromTableTranslate :: (IsSymbol name, Translate rest) => Translate (From (Table name fields) fields rest) where
      translate (From _ rest) = do
            q <- translate rest
            pure $ fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name) <> q

--as
--only when renaming a query
instance asTranslate :: IsSymbol name => Translate (As name E) where
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
instance whereTranslate :: (ToWhereFields c, Translate rest) => Translate (Where c rest) where
      translate (Where c rest) = do
            q <- toWhereFields c
            otherQ <- translate rest
            pure $ whereKeyword <> q <> otherQ

class ToWhereFields c where
      toWhereFields :: c -> State QueryState String

instance cToWhereFields :: (ToWhereFields a, ToWhereFields b) => ToWhereFields (Op a b) where
      toWhereFields (Op operator a b) = do
            q <- toWhereFields a
            otherQ <- toWhereFields b
            pure $
                  if operator == And || operator == Or then
                        openBracket <> q <> printOperator operator <> otherQ <> closeBracket
                   else
                        q <> printOperator operator <> otherQ

else instance c2ToWhereFields :: IsSymbol name => ToWhereFields (Proxy name) where
      toWhereFields name = pure $ DS.reflectSymbol name

else instance c3ToWhereFields :: (Append alias Dot path, Append path name fullPath,  IsSymbol fullPath) => ToWhereFields (Path alias name) where
      toWhereFields _ = pure $ DS.reflectSymbol (Proxy :: Proxy fullPath)

else instance c4ToWhereFields :: ToValue v => ToWhereFields v where
      toWhereFields p = do
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
instance insertTranslate :: (IsSymbol name, ToFieldNames fieldNames, ToFieldValues v, Translate rest) => Translate (Insert (Into name fields fieldNames (Values v rest))) where
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
instance updateTranslate :: (IsSymbol name, ToFieldValuePairs pairs, Translate rest) => Translate (Update name fields (Set pairs rest)) where
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
instance deleteTranslate :: Translate (From f fields rest) => Translate (Delete (From f fields rest)) where
      translate (Delete fr) = do
            q <- translate fr
            pure $ deleteKeyword <> q

--returning
instance returningTranslate :: (ToFieldNames fieldNames) => Translate (Returning fields fieldNames) where
      translate (Returning fieldNames) = pure $ returningKeyword <> toFieldNames fieldNames

--order by
instance orderByTranslate :: (ToSortNames f, Translate rest) => Translate (OrderBy f rest) where
      translate (OrderBy f rest) = do
            q <- translate rest
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
instance limitTranslate :: Translate rest => Translate (Limit rest) where
      translate (Limit n rest) = do
            q <- translate rest
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

quote :: forall alias. IsSymbol alias => Proxy alias -> String
quote name = quoteSymbol <> DS.reflectSymbol name <> quoteSymbol

quoteColumn :: forall name. IsSymbol name => Proxy name -> String
quoteColumn name = ref <> " " <> quoteSymbol <> ref <> quoteSymbol
      where ref =  DS.reflectSymbol name

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
