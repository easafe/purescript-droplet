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
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

data NakedSelect s = NakedSelect s

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

--trash
instance eToQuery :: ToQuery E projection is () where
      toQuery _ _ = NotParameterized ""

instance queryToQuery :: ToQuery (Query projection parameters) projection is parameters where
      toQuery q _ = q

--prepare
--quite a hack, stand to gain from cleaner implementation
instance prepareToQuery :: (ToQuery s projection Parameterized (), RowToList parameters list, ToNames list) => ToQuery (Prepare s parameters) projection is parameters where
      toQuery (Prepare s parameters plan) _ = Parameterized plan parameterList body parameters
            where body = DF.foldl replace (extract $ toQuery s (Proxy :: Proxy Parameterized)) $ DA.zip names indexes
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

--naked selects
instance intToQuery :: IsSymbol name => ToQuery (NakedSelect (As Int name)) projection is () where
      toQuery (NakedSelect (As n)) _ = NotParameterized $ show n <> asKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

else instance asNakedSelectToQuery :: (IsSymbol name, ToQuery s p is ()) => ToQuery (NakedSelect (Select s pp parameters (As E name))) ppp is () where
      toQuery (NakedSelect a) is = NotParameterized $ toAsQuery a is

else instance tupleToQuery :: (ToQuery (NakedSelect s) p is (), ToQuery (NakedSelect t) pp is ()) => ToQuery (NakedSelect (Tuple (Select s ss parameters E) (Select t tt parameters E))) ppp is () where
      toQuery (NakedSelect (Tuple (Select s _) (Select t _))) is = NotParameterized $ extract (toQuery (NakedSelect s) is) <> comma <> extract (toQuery (NakedSelect t) is)

else instance failNakedToQuery :: Fail (Text "Naked select columns must be either scalar values or named subqueries") => ToQuery (NakedSelect s) projection is () where
      toQuery _ _ = NotParameterized "impossible"

--this can be made a lot simpler
instance selectToQuery :: (
      ToQuery (NakedSelect s) pp is (),
      ToProjection s () projection,
      Nub projection unique,
      UniqueColumnNames projection unique
) => ToQuery (Select s p parameters E) unique is () where
      toQuery (Select s _) is = NotParameterized $ selectKeyword <> extract (toQuery (NakedSelect s) is)

--fully clothed selects
else instance asSelectToQuery :: (ToColumnQuery s is, ToQuery s pp is (), IsSymbol name) => ToQuery (Select s projection parameters (As E name)) projection is () where
      toQuery s is = NotParameterized $ toAsQuery s is

else instance fullSelectToQuery :: (ToColumnQuery s is, ToQuery rest p is ()) => ToQuery (Select s projection parameters rest) projection is () where
      toQuery (Select s rest) is = NotParameterized $ selectKeyword <> toColumnQuery s is <> extract (toQuery rest is)

class ToColumnQuery q (is :: IsParameterized)where
      toColumnQuery :: q -> Proxy is -> String

instance fieldToColumnQuery :: IsSymbol name => ToColumnQuery (Field name) is where
      toColumnQuery _ _ =  DS.reflectSymbol (Proxy :: Proxy name)

else instance tableToColumnQuery :: ToColumnQuery Star is  where
      toColumnQuery _ _ = starToken

else instance asIntToColumnQuery :: IsSymbol name => ToColumnQuery (As Int name) is where
      toColumnQuery (As n) _ =
            show n <>
            asKeyword <>
            DS.reflectSymbol (Proxy :: Proxy name)

else instance asFieldToColumnQuery :: (IsSymbol name, IsSymbol alias) => ToColumnQuery (As (Field name) alias) is where
      toColumnQuery _ _ =
            DS.reflectSymbol (Proxy :: Proxy name) <>
            asKeyword <>
            DS.reflectSymbol (Proxy :: Proxy alias)

else instance tupleToColumnQuery :: (ToColumnQuery s is, ToColumnQuery t is, ToQuery rest p is (), ToQuery extra pp is ()) => ToColumnQuery (Tuple (Select s some parameters rest) (Select t more parameters extra)) is where
      toColumnQuery (Tuple (Select s rest) (Select t extra)) is = toColumnQuery s is <> extract (toQuery rest is) <> comma <> toColumnQuery t is <> extract (toQuery extra is)

else instance asSelectToColumnQuery :: (ToColumnQuery s is, ToQuery s projection is (), IsSymbol name) => ToColumnQuery (Select s projection parameters (As E name)) is where
      toColumnQuery s is = toAsQuery s is

else instance elseToColumnQuery :: ToQuery q projection is () => ToColumnQuery q is where
      toColumnQuery q is = openBracket <> extract (toQuery q is) <> closeBracket

--from
instance fromTableToQuery :: (IsSymbol name, ToQuery rest p is ()) => ToQuery (From (Table name fields) fields rest) projection is () where
      toQuery (From _ rest) is = NotParameterized $ fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name) <> extract (toQuery rest is)

else instance fromAsToQuery :: (ToQuery s p is (), ToQuery rest pp is ()) => ToQuery (From s fields rest) projection is () where
      toQuery (From s rest) is = NotParameterized $ fromKeyword <> extract (toQuery s is) <> extract (toQuery rest is)

--where
instance whereFailToQuery :: Fail (Text "Parameters must be set. See Droplet.prepare") => ToQuery (Where Parameterized rest) projection NotParameterized () where
      toQuery _ _ = NotParameterized "impossible"

else instance whereToQuery :: ToQuery rest p is () => ToQuery (Where has rest) projection is () where
      toQuery (Where filtered rest) is = NotParameterized $ whereKeyword <> printFilter filtered <> extract (toQuery rest is)
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> equalsSymbol
                        NotEquals -> notEqualsSymbol

-- instance insertValuesToQuery :: ToQuery (InsertInto name fields fieldNames (Values fieldValues))

--helpers
toAsQuery :: forall name p s is parameters projection . IsSymbol name => ToQuery s p is () => Select s projection parameters (As E name) -> Proxy is -> String
toAsQuery (Select s (As E)) is =
      openBracket <>
      extract (toQuery s is) <>
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
