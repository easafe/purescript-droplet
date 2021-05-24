-- | `ToQuery`, a type class to generate parameterized SQL statement strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Driver` instead
module Droplet.Internal.Language.Query (class ToColumnQuery, class ToFieldNames, class ToSortNames, toSortNames, class ToFieldValuePairs, class ToFieldValues, class ToQuery, NakedSelect, Query(..), QueryState, toColumnQuery, toFieldNames, toFieldValuePairs, toFieldValues, toQuery, query, unsafeQuery) where

import Droplet.Internal.Language.Condition
import Droplet.Internal.Language.Definition
import Droplet.Internal.Language.Keyword
import Droplet.Internal.Language.Syntax
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
import Foreign (Foreign)
import Prim.Row (class Nub)
import Prim.RowList (class RowToList)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

data NakedSelect s = NakedSelect s

data Query (projection :: Row Type) = Query (Maybe Plan) String (Array Foreign)

type QueryState = { plan :: Maybe Plan, parameters :: Array Foreign}

instance queryShow :: Show (Query projection) where
      show (Query _ q _) = q

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
            CMS.put { plan: p, parameters }
            pure q

--prepare
instance prepareToQuery :: (ToQuery s projection) => ToQuery (Prepare s) projection where
      toQuery (Prepare s plan) = do
            CMS.modify_ (_ { plan = Just plan })
            toQuery s

--naked selects
instance intToQuery :: IsSymbol name => ToQuery (NakedSelect (As Int name)) projection where
      toQuery (NakedSelect (As n)) = pure $ show n <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

else instance asNakedSelectToQuery :: (IsSymbol name, ToQuery s p) => ToQuery (NakedSelect (Select s ss (As E name))) pp where
      toQuery (NakedSelect a) = toAsQuery a

else instance tupleToQuery :: (ToQuery (NakedSelect s) p, ToQuery (NakedSelect t) pp) => ToQuery (NakedSelect (Tuple (Select s ss E) (Select t tt E))) ppp where
      toQuery (NakedSelect (Tuple (Select s _) (Select t _))) = do
            q <- toQuery $ NakedSelect s
            otherQ <- toQuery $ NakedSelect t
            pure $ q <> comma <> otherQ

else instance failNakedToQuery :: Fail (Text "Naked select columns must be either scalar values or named subqueries") => ToQuery (NakedSelect s) projection where
      toQuery _ = pure "impossible"

--this can be made a lot simpler
instance selectToQuery :: (
      ToQuery (NakedSelect s) p,
      ToProjection s () projection,
      Nub projection unique,
      UniqueColumnNames projection unique
) => ToQuery (Select s pp E) unique where
      toQuery (Select s _) = do
            q <- toQuery $ NakedSelect s
            pure $ selectKeyword <> q

--fully clothed selects
else instance asSelectToQuery :: (ToColumnQuery s, ToQuery s pp, IsSymbol name) => ToQuery (Select s projection (As E name)) projection where
      toQuery s = toAsQuery s

else instance fullSelectToQuery :: (ToColumnQuery s, ToQuery rest p) => ToQuery (Select s projection rest) projection where
      toQuery (Select s rest) = do
            q <-  toColumnQuery s
            otherQ <- toQuery rest
            pure $ selectKeyword <> q <> otherQ

class ToColumnQuery q where
      toColumnQuery :: q -> State QueryState String

instance fieldToColumnQuery :: IsSymbol name => ToColumnQuery (Proxy name) where
      toColumnQuery name = pure $ DS.reflectSymbol name

else instance tableToColumnQuery :: ToColumnQuery Star where
      toColumnQuery _ = pure starToken

else instance asIntToColumnQuery :: IsSymbol name => ToColumnQuery (As Int name) where
      toColumnQuery (As n) = pure $ show n <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

else instance asFieldToColumnQuery :: (IsSymbol name, IsSymbol alias) => ToColumnQuery (As (Proxy name) alias) where
      toColumnQuery _ = pure $ DS.reflectSymbol (Proxy :: Proxy name) <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy alias)

else instance tupleToColumnQuery :: (ToColumnQuery s, ToColumnQuery t, ToQuery rest p, ToQuery extra pp) => ToColumnQuery (Tuple (Select s some rest) (Select t more extra)) where
      toColumnQuery (Tuple (Select s rest) (Select t extra)) = do
            sQ <- toColumnQuery s
            restQ <- toQuery rest
            tQ <- toColumnQuery t
            extraQ <- toQuery extra
            pure $ sQ <> restQ <> comma <> tQ <> extraQ

else instance asSelectToColumnQuery :: (ToQuery s projection, IsSymbol name) => ToColumnQuery (Select s projection (As E name)) where
      toColumnQuery s = toAsQuery s

else instance elseToColumnQuery :: ToQuery q projection => ToColumnQuery q where
      toColumnQuery q = do
            q <- toQuery q
            pure $ openBracket <> q <> closeBracket

--from
instance fromTableToQuery :: (IsSymbol name, ToQuery rest p) => ToQuery (From (Table name fields) fields rest) projection where
      toQuery (From _ rest) = do
            q <- toQuery rest
            pure $ fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name) <> q

--typing only s instead of (Select s p (As E name)) breaks purescript instance resolution
else instance fromAsToQuery :: (ToQuery (Select s p (As E name)) p, ToQuery rest pp) => ToQuery (From (Select s p (As E name)) fields rest) projection where
      toQuery (From s rest) = do
            q <- toQuery s
            otherQ <- toQuery rest
            pure $ fromKeyword <> q <> otherQ

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
instance deleteToQuery :: ToQuery (From f fields rest) p => ToQuery (Delete fields (From f fields rest)) () where
      toQuery (Delete fr) = do
            q <- toQuery fr
            pure $ deleteKeyword <> q

--returning
instance returningToQuery :: (ToFieldNames fieldNames, ToProjection fieldNames fields projection) => ToQuery (Returning fields fieldNames) projection where
      toQuery (Returning fieldNames) = pure $ returningKeyword <> toFieldNames fieldNames

--order by
instance orderByToQuery :: (ToSortNames f, ToQuery rest p) => ToQuery (OrderBy f fields rest) projection where
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


toAsQuery :: forall name p s projection. IsSymbol name => ToQuery s p => Select s projection (As E name) -> State QueryState String
toAsQuery (Select s (As _)) = do
      q <- toQuery s
      pure $ openBracket <> q <> closeBracket <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

query :: forall q projection. ToQuery q projection => q -> Query projection
query qr = Query plan q parameters
      where Tuple q {plan, parameters} = CMS.runState (toQuery qr) { plan: Nothing, parameters: [] }

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
            parameterIndexes = map (\i -> parameterToken <> show i) (1 .. DA.length parameterNames)
            replace sql (Tuple name p) = DSR.replace (DSRU.unsafeRegex (atToken <> "\\b" <> name <> "\\b") global) p sql
            dollaredQ = DA.foldl replace q $ DA.zip parameterNames parameterIndexes
