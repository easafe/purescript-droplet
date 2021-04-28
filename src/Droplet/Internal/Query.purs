-- | This module define `ToQuery`, a type class to generate parameterized SQL statements strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Query where

import Droplet.Internal.Definition
import Droplet.Internal.Filter
import Droplet.Internal.Language
import Prelude

import Data.Array ((..), (:))
import Data.Array as DA
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..), Replacement(..))
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

prepareKeyword :: String
prepareKeyword = "PREPARE "

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

data Statement

foreign import data Prepared :: Statement
foreign import data Other :: Statement

newtype NakedSelect s = NakedSelect s

--cant be a functor because of parameters kind
data Query parameters =
      Parameterized String (Maybe String) String (Record parameters) | -- keyword parameter list body, dont include name as it has to be unique per session
      Plain String

--for debugging
instance queryShow :: Show (Query parameters) where
      show = case _ of
            Parameterized q r s _ -> q <> "plan" <> DM.fromMaybe "" r <> s
            Plain q -> q

-- use this instead of toQuery
query :: forall q parameters. ToQuery q Other => q -> Query parameters
query q = toQuery q (Proxy :: Proxy Other)

--any way to not have to pass the proxy around?
class ToQuery q (starting :: Statement) where
      toQuery :: forall parameters. q -> Proxy starting -> Query parameters

{-

ToQuery should print valid sql strings but reject queries that can't be executed, with the following caveats

1. naked selects (i.e. not projecting from a source) can be potentially invalid (e.g. SELECT id) so only a limited sub set of SELECT is accepted as top level

2. parameters are changed from @name to $n format
      ToQuery rejects any query that includes parameters but no PREPARE statement

2. PREPARE statements are not fully printed here
      PREPARE names must be unique per session

-}

----------------------PREPARE----------------------------

--quite a hack, stand to gain from cleaner implementation
instance prepareToQuery :: (ToQuery q Prepared, RowToList parameters list, ToNames list) => ToQuery (Prepare q parameters) starting where
      toQuery (Prepare q parameters) _ = Parameterized prepareKeyword parameterList body $ UC.unsafeCoerce parameters
            where body = DF.foldl replace (extractPlain $ toQuery q (Proxy :: Proxy Prepared)) $ DA.zip names indexes
                  parameterList = Just <<< DST.joinWith comma $ map (atToken <> _) names -- to help debugging

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
instance intToQuery :: ToQuery (NakedSelect Int) starting where
      toQuery (NakedSelect n) _ = Plain $ show n
else
instance asSelectToQuery :: (IsSymbol name, ToQuery q starting) => ToQuery (NakedSelect (As q name parameters projection)) starting where
      toQuery (NakedSelect a) s = Plain $ toAsQuery a s
else
instance tupleToQuery :: (ToQuery (NakedSelect s) starting, ToQuery (NakedSelect t) starting) => ToQuery (NakedSelect (Tuple (Select s parameters) (Select t parameters))) starting where
      toQuery (NakedSelect (Tuple (Select s) (Select t))) st = Plain $ extractPlain (toQuery (NakedSelect s) st) <> comma <> extractPlain (toQuery (NakedSelect t) st)
else
instance failNakedToQuery :: Fail (Text "Naked select columns must be either scalar values or named subqueries") => ToQuery (NakedSelect s) starting where
      toQuery _ _ = Plain "impossible"

instance selectToQuery :: (
      ToQuery (NakedSelect s) starting,
      ToProjection s () fields,
      Nub fields unique,
      UniqueColumnNames fields unique
) => ToQuery (Select s parameters) starting where
      toQuery (Select s) st = Plain $ selectKeyword <> extractPlain (toQuery (NakedSelect s) st)

--fully clothed selects
class ToSelectQuery q (starting :: Statement) where
      toSelectQuery :: forall parameters. q -> Proxy starting -> Query parameters

instance asToSelectQuery :: (IsSymbol name, ToQuery q starting) => ToSelectQuery (As q name parameters projection) starting where
      toSelectQuery a s = Plain $ toAsQuery a s

instance selectToSelectQuery :: ToSelectQuery s starting => ToSelectQuery (Select s parameters) starting where
      toSelectQuery (Select s) st = Plain $ selectKeyword <> extractPlain (toSelectQuery s st)

instance fieldToSelectQuery :: IsSymbol name => ToSelectQuery (Field name) starting where
      toSelectQuery _ _ = Plain $ DS.reflectSymbol (Proxy :: Proxy name)

instance tableToSelectQuery :: ToSelectQuery Star starting where
      toSelectQuery _ _ = Plain starToken

instance intToSelectQuery :: ToSelectQuery Int starting where
      toSelectQuery n _ = Plain $ show n

instance tupleToSelectQuery :: (ToSelectQuery s starting, ToSelectQuery t starting) => ToSelectQuery (Tuple (Select s parameters) (Select t parameters)) starting where
      toSelectQuery (Tuple (Select s) (Select t)) st = Plain $ extractPlain (toSelectQuery s st)  <> comma <> extractPlain (toSelectQuery t st)



-------------------------------FROM----------------------------


instance fromTableToQuery :: (IsSymbol name, ToSelectQuery s starting) => ToQuery (From (Table name fields) s parameters fields) starting where
      toQuery (From _ s) st = Plain $ extractPlain (toSelectQuery s st) <> fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

instance fromAsToQuery :: (ToQuery q starting, ToSelectQuery s starting, IsSymbol name) => ToQuery (From (As q name parameters projection) s parameters projection) starting where
      toQuery (From a s) st = Plain $
            extractPlain (toSelectQuery s st) <>
            fromKeyword <>
            toAsQuery a st


-------------------------------WHERE----------------------------

instance whereFailToQuery :: Fail (Text "Parameters must be set. See Droplet.prepare") => ToQuery (Where f Parameterized parameters) Other where
      toQuery _ _ = Plain "impossible"
else
instance whereToQuery :: ToQuery f starting => ToQuery (Where f has parameters) starting where
      toQuery (Where filtered fr) st = Plain $ extractPlain (toQuery fr st) <> whereKeyword <> printFilter filtered
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> equalsSymbol
                        NotEquals -> notEqualsSymbol



toAsQuery :: forall name q starting parameters projection. IsSymbol name => ToQuery q starting => As q name parameters projection -> Proxy starting -> String
toAsQuery (As q) st =
      openBracket <>
      extractPlain (toQuery q st) <>
      closeBracket <>
      asKeyword <>
      DS.reflectSymbol (Proxy :: Proxy name)

extractPlain :: forall parameters. Query parameters -> String
extractPlain = case _ of
      Plain query -> query
      _ -> EEU.unsafeThrow "Tried to append to parameterized query"
