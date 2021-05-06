-- | This module define `ToQuery`, a type class to generate parameterized SQL statements strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Mapper.Query where

import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Filter
import Droplet.Internal.Edsl.Language
import Prelude

import Data.Array ((..), (:))
import Data.Array as DA
import Data.Foldable as DF
import Data.String as DST
import Data.String.Regex as DSR
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe as DSRU
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe as EEU
import Prim.Row (class Nub)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

newtype NakedSelect s = NakedSelect s

--cant be a functor because of parameters kind
data Query (projection :: Row Type) parameters =
      Parameterized Plan String String (Record parameters) | -- plan name, parameter names for debugging, body and parameters
      NotParameterized String

--for debugging
instance queryShow :: Show (Query projection parameters) where
      show = case _ of
            Parameterized _ r s _ -> r <> s
            NotParameterized q -> q

-- use this instead of toQuery
query :: forall q projection parameters. ToQuery q projection NotParameterized parameters => q -> Query projection parameters
query q = toQuery q (Proxy :: Proxy NotParameterized)

class ToQuery q projection (is :: IsParameterized) parameters | q -> projection, q -> parameters where
      toQuery :: q -> Proxy is -> Query projection parameters

{-

ToQuery should print valid sql strings but reject queries that can't be executed, with the following caveats

1. naked selects (i.e. not projecting from a source) can be potentially invalid (e.g. SELECT id) so only a limited sub set of SELECT is accepted as top level

2. parameters are changed from @name to $n format
      ToQuery rejects any query that includes parameters but no PREPARE

2. PREPARE statements are not printed here
      pg will do it for us

-}

instance queryToQuery :: ToQuery (Query projection parameters) projection is parameters where
      toQuery q _ = q

----------------------PREPARE----------------------------

--quite a hack, stand to gain from cleaner implementation
instance prepareToQuery :: (ToQuery q projection Parameterized (), RowToList parameters list, ToNames list) => ToQuery (Prepare q parameters) projection is parameters where
      toQuery (Prepare q parameters plan) _ = Parameterized plan parameterList body parameters
            where body = DF.foldl replace (extract $ toQuery q (Proxy :: Proxy Parameterized)) $ DA.zip names indexes
                  parameterList = DST.joinWith comma $ map (atToken <> _) names -- to help debugging

                  names = toNames (Proxy :: Proxy list)
                  indexes = map (\i -> parameterToken <> show i) (1 .. DA.length names)
                  replace sql (Tuple name p) = DSR.replace (DSRU.unsafeRegex (atToken <> "\\b" <> name <> "\\b") global) p sql

class ToNames (list :: RowList Type) where
      toNames :: Proxy list -> Array String

instance nilToNames :: ToNames RL.Nil where
      toNames _ = []

instance consToNames :: (IsSymbol name, ToNames rest) => ToNames (RL.Cons name t rest) where
      toNames _ = DS.reflectSymbol (Proxy :: Proxy name) : toNames (Proxy :: Proxy rest)



----------------------SELECT----------------------------

--naked selects
instance intToQuery :: IsSymbol name => ToQuery (NakedSelect (As Int name p pp)) projection is () where
      toQuery (NakedSelect (As n)) _ = NotParameterized $ show n <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)
else
instance asSelectToQuery :: (IsSymbol name, ToQuery q p is ()) => ToQuery (NakedSelect (As q name parameters projection)) p is () where
      toQuery (NakedSelect a) s = NotParameterized $ toAsQuery a s
else
instance tupleToQuery :: (ToQuery (NakedSelect s) p is (), ToQuery (NakedSelect t) pp is ()) => ToQuery (NakedSelect (Tuple (Select s parameters) (Select t parameters))) projection is () where
      toQuery (NakedSelect (Tuple (Select s) (Select t))) is = NotParameterized $ extract (toQuery (NakedSelect s) is) <> comma <> extract (toQuery (NakedSelect t) is)
else
instance failNakedToQuery :: Fail (Text "Naked select columns must be either scalar values or named subqueries") => ToQuery (NakedSelect s) projection is () where
      toQuery _ _ = NotParameterized "impossible"

instance selectToQuery :: (
      ToQuery (NakedSelect s) u is (),
      ToProjection s () fields,
      Nub fields unique,
      UniqueColumnNames fields unique
) => ToQuery (Select s parameters) unique is () where
      toQuery (Select s) is = NotParameterized $ selectKeyword <> extract (toQuery (NakedSelect s) is)

--fully clothed selects
class ToSelectQuery q (is :: IsParameterized) (parameters :: Row Type) | q -> parameters where
      toSelectQuery :: q -> Proxy is -> String


instance selectToSelectQuery :: ToSelectQuery s is () => ToSelectQuery (Select s parameters) is () where
      toSelectQuery (Select s) is = selectKeyword <> toSelectQuery s is

else instance fieldToSelectQuery :: IsSymbol name => ToSelectQuery (Field name) is () where
      toSelectQuery _ _ =  DS.reflectSymbol (Proxy :: Proxy name)

else instance tableToSelectQuery :: ToSelectQuery Star is () where
      toSelectQuery _ _ = starToken

else instance tupleToSelectQuery :: (ToSelectQuery s is (), ToSelectQuery t is ()) => ToSelectQuery (Tuple (Select s parameters) (Select t parameters)) is () where
      toSelectQuery (Tuple (Select s) (Select t)) is = toSelectQuery s is <> comma <> toSelectQuery t is

else instance asIntToSelectQuery :: IsSymbol name => ToSelectQuery (As Int name parameters projection) is () where
      toSelectQuery (As n) _ =
            show n <>
            asKeyword <>
            DS.reflectSymbol (Proxy :: Proxy name)

else instance asFieldToSelectQuery :: (IsSymbol name, IsSymbol alias) => ToSelectQuery (As (Field name) alias parameters projection) is () where
      toSelectQuery _ _ =
            DS.reflectSymbol (Proxy :: Proxy name) <>
            asKeyword <>
            DS.reflectSymbol (Proxy :: Proxy alias)

else instance asToSelectQuery :: (IsSymbol name, ToQuery q projection is ()) => ToSelectQuery (As q name parameters projection) is () where
      toSelectQuery a is = toAsQuery a is

else instance elseToSelectQuery :: ToQuery q projection is () => ToSelectQuery q is () where
      toSelectQuery q is = openBracket <> extract (toQuery q is) <> closeBracket



-------------------------------FROM----------------------------

instance fromTableToQuery :: (
      ToSelectQuery s is (),
      IsSymbol name,
      ToProjection s fields projection
) => ToQuery (From (Table name fields) s parameters fields) projection is () where
      toQuery (From _ s) is = NotParameterized $ toSelectQuery s is <> fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

instance fromAsToQuery :: (
      ToSelectQuery s is (),
      ToQuery q p is (),
      IsSymbol name,
      ToProjection s p projection
) => ToQuery (From (As q name parameters p) s parameters fields) projection is () where
      toQuery (From a s) is = NotParameterized $
            toSelectQuery s is <>
            fromKeyword <>
            toAsQuery a is


-------------------------------WHERE----------------------------

instance whereFailToQuery :: Fail (Text "Parameters must be set. See Droplet.prepare") => ToQuery (Where f Parameterized parameters) projection NotParameterized () where
      toQuery _ _ = NotParameterized "impossible"
else
instance whereToQuery :: ToQuery f projection is () => ToQuery (Where f has parameters) projection is () where
      toQuery (Where filtered fr) is = NotParameterized $ extract (toQuery fr is) <> whereKeyword <> printFilter filtered
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> equalsSymbol
                        NotEquals -> notEqualsSymbol

toAsQuery :: forall name p q is parameters projection . IsSymbol name => ToQuery q p is () => As q name parameters projection -> Proxy is -> String
toAsQuery (As q) is =
      openBracket <>
      extract (toQuery q is) <>
      closeBracket <>
      asKeyword <>
      DS.reflectSymbol (Proxy :: Proxy name)

extract :: forall projection parameters. Query projection parameters -> String
extract = case _ of
      NotParameterized query -> query
      _ -> EEU.unsafeThrow "Tried to append to parameterized query"

--magic strings
selectKeyword :: String
selectKeyword = "SELECT "

fromKeyword :: String
fromKeyword = " FROM "

whereKeyword :: String
whereKeyword = " WHERE "

andKeyword :: String
andKeyword = " AND "

orKeyword :: String
orKeyword = " OR "

asKeyword :: String
asKeyword = " AS "

starToken :: String
starToken = "*"

comma :: String
comma = ", "

openBracket :: String
openBracket = "("

closeBracket :: String
closeBracket = ")"

equalsSymbol :: String
equalsSymbol = " = "

notEqualsSymbol :: String
notEqualsSymbol = " <> "

parameterToken :: String
parameterToken = "$"
