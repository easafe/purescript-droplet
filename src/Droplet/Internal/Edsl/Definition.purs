module Droplet.Internal.Edsl.Definition where

import Prelude

import Control.Monad.Except as CME
import Data.Array ((:))
import Data.Bifunctor as DB
import Data.Date (Date)
import Data.Date as DD
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either(..))
import Data.Either as DE
import Data.Enum as DEN
import Data.Int as DI
import Data.String (Pattern(..))
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Foreign (Foreign)
import Foreign as F
import Prim.Row (class Cons)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record as R
import Type.Proxy (Proxy(..))

data Field (name :: Symbol) = Field

data Alias (name :: Symbol) = Alias

data Star = Star

star :: Star
star = Star

data Table (name :: Symbol) (fields :: Row Type) = Table



class ToValue v where
      toValue :: v -> Foreign

instance stringToValue :: ToValue String where
      toValue = F.unsafeToForeign

instance intToValue :: ToValue Int where
      toValue = F.unsafeToForeign

instance dateToValue :: ToValue Date where
      toValue = F.unsafeToForeign <<< formatDate

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

