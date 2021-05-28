-- | Definition of SQL columns types as well conversions from and to columns
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Definition (class FromValue, class InvalidField, class ToParameters, class ToValue, class UnwrapDefinition, Auto(..), Default(..), Star(..), Table(..), star, toParameters, fromValue, toValue, dot, (...), Dot) where

import Prelude

import Control.Monad.Except as CME
import Data.Array ((:))
import Data.Bifunctor as DB
import Data.BigInt (BigInt)
import Data.BigInt as DBT
import Data.Date (Date)
import Data.Date as DD
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either(..))
import Data.Either as DE
import Data.Either as DET
import Data.Enum as DEN
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Nullable as DN
import Data.String (Pattern(..))
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign (Foreign)
import Foreign as F
import Prim.Row (class Cons)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Record as R
import Type.Proxy (Proxy(..))

data Star = Star

star :: Star
star = Star

newtype Auto a = Auto a

newtype Default a = Default a

data Table (name :: Symbol) (fields :: Row Type) = Table

data Dot (field :: Symbol) = Dot

dot :: forall alias field path pathField . Append alias "." path => Append path field pathField => Proxy alias -> Proxy field -> Dot pathField
dot _ _ = Dot

infix 5 dot as ...

derive instance defaultEq :: Eq a => Eq (Default a)

derive instance autoEq :: Eq a => Eq (Auto a)

instance defaultShow :: Show a => Show (Default a) where
      show (Default a) = show a

instance autoShow :: Show a => Show (Auto a) where
      show (Auto a) = show a


class ToValue v where
      toValue :: v -> Foreign

instance stringToValue :: ToValue String where
      toValue = F.unsafeToForeign

instance intToValue :: ToValue Int where
      toValue = F.unsafeToForeign

instance booleanToValue :: ToValue Boolean where
      toValue = F.unsafeToForeign

instance numberToValue :: ToValue Number where
      toValue = F.unsafeToForeign

instance defaultToValue :: ToValue a => ToValue (Default a) where
      toValue (Default a) = toValue a

instance autoToValue :: ToValue a => ToValue (Auto a) where
      toValue (Auto a) = toValue a

instance maybeToValue :: ToValue a => ToValue (Maybe a) where
      toValue = case _ of
            Nothing -> F.unsafeToForeign DN.null
            Just a -> toValue a

instance dateToValue :: ToValue Date where
      toValue = F.unsafeToForeign <<< formatDate

instance arrayToValue :: ToValue a => ToValue (Array a) where
      toValue = F.unsafeToForeign <<< map toValue

instance dateTimeToValue :: ToValue DateTime where
      toValue (DateTime date (Time h m s ms)) = F.unsafeToForeign $ formatDate date <> "T" <> time <> "+0000"
            where time = show (DEN.fromEnum h) <> ":" <> show (DEN.fromEnum m) <> ":" <> show (DEN.fromEnum s) <> "." <> show (DEN.fromEnum ms)

formatDate :: Date -> String
formatDate date = show y <> "-" <> show m <> "-" <> show d
      where y = DEN.fromEnum $ DD.year date
            m = DEN.fromEnum $ DD.month date
            d = DEN.fromEnum $ DD.day date


class FromValue t where
      fromValue :: Foreign -> Either String t

instance intFromValue :: FromValue Int where
      fromValue = DB.lmap show <<< CME.runExcept <<< F.readInt

instance stringFromValue :: FromValue String where
      fromValue = DB.lmap show <<< CME.runExcept <<< F.readString

instance booleanFromValue :: FromValue Boolean where
      fromValue = DB.lmap show <<< CME.runExcept <<< F.readBoolean

instance numberFromValue :: FromValue Number where
      fromValue = DB.lmap show <<< CME.runExcept <<< F.readNumber

instance defaultFromValue :: FromValue v => FromValue (Default v) where
      fromValue v = Default <$> fromValue v

instance autoFromValue :: FromValue v => FromValue (Auto v) where
      fromValue v = Auto <$> fromValue v

instance arrayFromValue :: FromValue v => FromValue (Array v) where
      fromValue = DT.traverse fromValue <=< DB.lmap show <<< CME.runExcept <<< F.readArray

instance bigIntFromValue :: FromValue BigInt where
      fromValue v = do
            i <- DB.lmap show <<< CME.runExcept $ F.readString v
            DET.note ("Could not parse big int from " <> i) $ DBT.fromString i

instance maybeFromValue :: FromValue v => FromValue (Maybe v) where
      fromValue v
            | F.isNull v = pure Nothing
            | otherwise = Just <$> fromValue v

instance dateFromValue :: FromValue Date where
      fromValue v = do
            s <- DB.lmap show <<< CME.runExcept $ F.readString v
            parseDate s $ "ISO 8601 date parsing failed for value: " <> s

instance dateTimeFromValue :: FromValue DateTime where
      fromValue v = do
            s <- DB.lmap show <<< CME.runExcept $ F.readString v
            let errorMessage = "ISO 8601 date time parsing failed for value: " <> s
            case DST.split (Pattern " ") s of
                  [datePart, timePart] -> do
                        date <- parseDate datePart errorMessage
                        time <- parseTime timePart errorMessage
                        Right $ DateTime date time
                  _ -> Left errorMessage

parseDate :: String -> String -> Either String Date
parseDate input errorMessage =
      case DST.split (Pattern "-") input of
            [y, m, d] -> do
                  let result = DD.canonicalDate <$> (DEN.toEnum =<< DI.fromString y) <*> (DEN.toEnum =<< DI.fromString m) <*> (DEN.toEnum =<< DI.fromString d)
                  DE.note errorMessage result
            _ -> Left errorMessage

parseTime :: String -> String -> Either String Time
parseTime input errorMessage =
      case DST.split (Pattern ":") input of
            [h, m, s] -> do
                  let result = Time <$> (DEN.toEnum =<< DI.fromString h) <*> (DEN.toEnum =<< DI.fromString m) <*> (DEN.toEnum =<< DI.fromString (DST.take 2 s)) <*> (DEN.toEnum 0)
                  DE.note errorMessage result
            _ -> Left errorMessage


class UnwrapDefinition (w :: Type) (t :: Type) | w -> t

instance autoUnwrapDefinition :: UnwrapDefinition (Auto t) t

else instance defaultUnwrapDefinition :: UnwrapDefinition (Default t) t

else instance elseUnwrapDefinition :: UnwrapDefinition t t


class InvalidField (t :: Type)

instance autoInvalidField :: Fail (Text "Auto columns cannot be inserted or updated") => InvalidField (Auto t)

else instance elseInvalidField :: InvalidField t


class ToParameters record (list :: RowList Type) where
      toParameters :: Proxy list -> Record record -> Array (Tuple String Foreign)

instance nilToParameters :: ToParameters record RL.Nil where
      toParameters _ _ = []

instance consToParameters :: (
      IsSymbol name,
      ToValue t,
      Cons name t e record,
      ToParameters record rest
) => ToParameters record (RL.Cons name t rest) where
      toParameters _ record = (DS.reflectSymbol name /\ toValue (R.get name record)) : toParameters (Proxy :: Proxy rest) record
            where name = Proxy :: Proxy name
