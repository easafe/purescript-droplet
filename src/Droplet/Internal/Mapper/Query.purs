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
import Unsafe.Coerce as UC

newtype NakedSelect s = NakedSelect s

--cant be a functor because of parameters kind
data Query (projection :: Row Type) parameters =
      Parameterized Plan String String (Record parameters) | -- plan name, parameter names for debugging, body and parameters
      Plain String

--for debugging
instance queryShow :: Show (Query projection parameters) where
      show = case _ of
            Parameterized _ r s _ -> r <> s
            Plain q -> q

-- use this instead of toQuery
query :: forall q projection parameters. ToQuery q projection NotParameterized parameters => q -> Query projection parameters
query q = toQuery q (Proxy :: Proxy NotParameterized)

--any way to not have to pass the proxy around?
-- we prolly can work something here with parameters row list to get rid of IsParameterized and not need to annotate types of queries without parameters
class ToQuery q projection (is :: IsParameterized) parameters | q -> projection, q -> parameters where
      toQuery :: q -> Proxy is -> Query projection parameters

{-

ToQuery should print valid sql strings but reject queries that can't be executed, with the following caveats

1. naked selects (i.e. not projecting from a source) can be potentially invalid (e.g. SELECT id) so only a limited sub set of SELECT is accepted as top level

2. parameters are changed from @name to $n format
      ToQuery rejects any query that includes parameters but no PREPARE statement

2. PREPARE statements are not printed here
      pg will do it for us

-}

----------------------PREPARE----------------------------

--quite a hack, stand to gain from cleaner implementation
instance prepareToQuery :: (ToQuery q projection Parameterized parameters, RowToList parameters list, ToNames list) => ToQuery (Prepare q parameters) projection is parameters where
      toQuery (Prepare q parameters plan) _ = Parameterized plan parameterList body $ UC.unsafeCoerce parameters
            where body = DF.foldl replace (extractPlain $ toQuery q (Proxy :: Proxy Parameterized)) $ DA.zip names indexes
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
instance intToQuery :: IsSymbol name => ToQuery (NakedSelect (As Int name p pp)) projection is prmt where
      toQuery (NakedSelect (As n)) _ = Plain $ show n <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)
else
instance asSelectToQuery :: (IsSymbol name, ToQuery q p is prmt) => ToQuery (NakedSelect (As q name parameters projection)) p is prmt where
      toQuery (NakedSelect a) s = Plain $ toAsQuery a s
else
instance tupleToQuery :: (ToQuery (NakedSelect s) p is prmt, ToQuery (NakedSelect t) pp is prmt) => ToQuery (NakedSelect (Tuple (Select s parameters) (Select t parameters))) projection is prmt where
      toQuery (NakedSelect (Tuple (Select s) (Select t))) is = Plain $ extractPlain (toQuery (NakedSelect s) is) <> comma <> extractPlain (toQuery (NakedSelect t) is)
else
instance failNakedToQuery :: Fail (Text "Naked select columns must be either scalar values or named subqueries") => ToQuery (NakedSelect s) projection is prmt where
      toQuery _ _ = Plain "impossible"

instance selectToQuery :: (
      ToQuery (NakedSelect s) u is prmt,
      ToProjection s () fields,
      Nub fields unique,
      UniqueColumnNames fields unique
) => ToQuery (Select s parameters) unique is prmt where
      toQuery (Select s) is = Plain $ selectKeyword <> extractPlain (toQuery (NakedSelect s) is)

--fully clothed selects
class ToSelectQuery q (is :: IsParameterized) prmt | q -> prmt where
      toSelectQuery :: q -> Proxy is -> String

--we likely want more instances of named columns and abstrac it with a function
instance asIntToSelectQuery :: IsSymbol name => ToSelectQuery (As Int name parameters projection) is prmt where
      toSelectQuery (As n) _ =
            show n <>
            asKeyword <>
            DS.reflectSymbol (Proxy :: Proxy name)
else
instance asToSelectQuery :: (IsSymbol name, ToQuery q projection is prmt) => ToSelectQuery (As q name parameters projection) is prmt where
      toSelectQuery a is = toAsQuery a is

instance selectToSelectQuery :: ToSelectQuery s is prmt => ToSelectQuery (Select s parameters) is prmt where
      toSelectQuery (Select s) is = selectKeyword <> toSelectQuery s is

instance fieldToSelectQuery :: IsSymbol name => ToSelectQuery (Field name) is prmt where
      toSelectQuery _ _ =  DS.reflectSymbol (Proxy :: Proxy name)

instance tableToSelectQuery :: ToSelectQuery Star is prmt where
      toSelectQuery _ _ = starToken

instance tupleToSelectQuery :: (ToSelectQuery s is prmt, ToSelectQuery t is prmt) => ToSelectQuery (Tuple (Select s parameters) (Select t parameters)) is prmt where
      toSelectQuery (Tuple (Select s) (Select t)) is = toSelectQuery s is <> comma <> toSelectQuery t is



-------------------------------FROM----------------------------

instance fromTableToQuery :: (
      ToSelectQuery s is prmt,
      IsSymbol name,
      ToProjection s fields projection
) => ToQuery (From (Table name fields) s parameters fields) projection is prmt where
      toQuery (From _ s) is = Plain $ toSelectQuery s is <> fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

instance fromAsToQuery :: (
      ToSelectQuery s is prmt,
      ToQuery q p is prmt,
      IsSymbol name,
      ToProjection s projection p
) => ToQuery (From (As q name parameters projection) s parameters projection) p is prmt where
      toQuery (From a s) is = Plain $
            toSelectQuery s is <>
            fromKeyword <>
            toAsQuery a is


-------------------------------WHERE----------------------------

instance whereFailToQuery :: Fail (Text "Parameters must be set. See Droplet.prepare") => ToQuery (Where f Parameterized parameters) projection NotParameterized prmt where
      toQuery _ _ = Plain "impossible"
else
instance whereToQuery :: ToQuery f projection is prmt => ToQuery (Where f has parameters) projection is prmt where
      toQuery (Where filtered fr) is = Plain $ extractPlain (toQuery fr is) <> whereKeyword <> printFilter filtered
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> equalsSymbol
                        NotEquals -> notEqualsSymbol

toAsQuery :: forall name p q is parameters projection prmt. IsSymbol name => ToQuery q p is prmt=> As q name parameters projection -> Proxy is -> String
toAsQuery (As q) is =
      openBracket <>
      extractPlain (toQuery q is) <>
      closeBracket <>
      asKeyword <>
      DS.reflectSymbol (Proxy :: Proxy name)

extractPlain :: forall projection parameters. Query projection parameters -> String
extractPlain = case _ of
      Plain query -> query
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
