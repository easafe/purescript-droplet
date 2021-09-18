-- | Definition of SQL columns types as well conversions from and to columns
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Definition (class FromValue, Empty, class InvalidField, class UnwrapNullable, class ToParameters, class ToValue, class UnwrapDefinition, Auto(..), Default(..), Star(..), Table(..), star, toParameters, fromValue, toValue, Joined(..), path, (...), E(..), Path, class AppendPath) where

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
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Data.String (Pattern(..))
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Keyword (Dot, dotSymbol)
import Foreign (Foreign)
import Foreign as F
import Prim.Row (class Cons)
import Prim.RowList (RowList, Cons, Nil)
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Text)
import Record as R
import Type.Proxy (Proxy(..))

foreign import readInt ∷ Foreign → Nullable Int
foreign import showForeigner ∷ Foreign → String

-- | Marks the query end
data E = E

type Empty = ""

data Star = Star

star ∷ Star
star = Star

-- | Identity fields
data Auto (a :: Type)

-- | Default constraints
data Default (a :: Type) = Default

-- | A trick to mark left joined columns as nullable
data Joined (a :: Type)

data Table (name ∷ Symbol) (fields ∷ Row Type) = Table

-- | Qualified columns (e.g, table.column)
data Path (alias ∷ Symbol) (field ∷ Symbol) = Path

path ∷ ∀ alias field path pathField. Append alias Dot path ⇒ Append path field pathField ⇒ Proxy alias → Proxy field → Path alias field
path _ _ = Path

infix 7 path as ...

derive instance Eq a ⇒ Eq (Default a)

derive instance Eq a ⇒ Eq (Auto a)

class ToValue v where
      toValue ∷ v → Foreign

instance ToValue Int where
      toValue = F.unsafeToForeign

instance ToValue String where
      toValue = F.unsafeToForeign

instance ToValue (Default t) where
      toValue _ = F.unsafeToForeign $ DN.null

instance ToValue Boolean where
      toValue = F.unsafeToForeign

instance ToValue Number where
      toValue = F.unsafeToForeign

instance ToValue a ⇒ ToValue (Maybe a) where
      toValue = case _ of
            Nothing → F.unsafeToForeign DN.null
            Just a → toValue a

instance ToValue BigInt where
      toValue = F.unsafeToForeign <<< DBT.toString

instance ToValue Date where
      toValue = F.unsafeToForeign <<< formatDate

instance ToValue a ⇒ ToValue (Array a) where
      toValue = F.unsafeToForeign <<< map toValue

instance ToValue DateTime where
      toValue (DateTime date (Time h m s ms)) = F.unsafeToForeign $ formatDate date <> "t" <> time <> "+0000"
            where
            time = show (DEN.fromEnum h) <> ":" <> show (DEN.fromEnum m) <> ":" <> show (DEN.fromEnum s) <> dotSymbol <> show (DEN.fromEnum ms)

formatDate ∷ Date → String
formatDate date = show y <> "-" <> show m <> "-" <> show d
      where
      y = DEN.fromEnum $ DD.year date
      m = DEN.fromEnum $ DD.month date
      d = DEN.fromEnum $ DD.day date

class FromValue t where
      fromValue ∷ Foreign → Either String t

--sometimes node pg returns a string for integers
-- this might arise out a invalid type definition on the users part;
-- the number is actually a big int;
-- something funky
--in the two former cases, readInt returns null, as well in the latter if the string can't be parsed as an integer
instance FromValue Int where
      fromValue i = case DN.toMaybe $ readInt i of
            Nothing → Left $ "Could not parse value as integer: " <> showForeigner i
            Just int → Right int

instance FromValue String where
      fromValue = DB.lmap show <<< CME.runExcept <<< F.readString

instance FromValue Boolean where
      fromValue = DB.lmap show <<< CME.runExcept <<< F.readBoolean

instance FromValue Number where
      fromValue = DB.lmap show <<< CME.runExcept <<< F.readNumber

--tricky, since pg might return empty string for select some_side_effect_function()
instance FromValue Unit where
      fromValue _ = Right unit

instance FromValue v ⇒ FromValue (Array v) where
      fromValue = DT.traverse fromValue <=< DB.lmap show <<< CME.runExcept <<< F.readArray

instance FromValue BigInt where
      fromValue v = do
            i ← DB.lmap show <<< CME.runExcept $ F.readString v
            DET.note ("Could not parse big int from " <> i) $ DBT.fromString i

instance FromValue v ⇒ FromValue (Maybe v) where
      fromValue v
            | F.isNull v = pure Nothing
            | otherwise = Just <$> fromValue v

instance FromValue Date where
      fromValue v = do
            s ← DB.lmap show <<< CME.runExcept $ F.readString v
            parseDate s $ "ISO 8601 date parsing failed for value: " <> s

instance FromValue DateTime where
      fromValue v = do
            s ← DB.lmap show <<< CME.runExcept $ F.readString v
            let errorMessage = "ISO 8601 date time parsing failed for value: " <> s
            case DST.split (Pattern " ") s of
                  [ datePart, timePart ] → do
                        date ← parseDate datePart errorMessage
                        time ← parseTime timePart errorMessage
                        Right $ DateTime date time
                  _ → Left errorMessage

parseDate ∷ String → String → Either String Date
parseDate input errorMessage =
      case DST.split (Pattern "-") input of
            [ y, m, d ] → do
                  let result = DD.canonicalDate <$> (DEN.toEnum =<< DI.fromString y) <*> (DEN.toEnum =<< DI.fromString m) <*> (DEN.toEnum =<< DI.fromString d)
                  DE.note errorMessage result
            _ → Left errorMessage

parseTime ∷ String → String → Either String Time
parseTime input errorMessage =
      case DST.split (Pattern ":") input of
            [ h, m, s ] → do
                  let result = Time <$> (DEN.toEnum =<< DI.fromString h) <*> (DEN.toEnum =<< DI.fromString m) <*> (DEN.toEnum =<< DI.fromString (DST.take 2 s)) <*> (DEN.toEnum 0)
                  DE.note errorMessage result
            _ → Left errorMessage

-- | Convenience to remove type wrappers
class UnwrapDefinition (w ∷ Type) (t ∷ Type) | w → t

instance UnwrapDefinition (Auto t) t

else instance UnwrapDefinition (Default t) t

else instance UnwrapDefinition t u ⇒ UnwrapDefinition (Joined t) u

else instance UnwrapDefinition t t

class UnwrapNullable (w ∷ Type) (t ∷ Type) | w → t

instance UnwrapNullable (Maybe t) t

else instance UnwrapNullable t t

class InvalidField (t ∷ Type)

instance Fail (Text "Auto columns cannot be inserted or updated") ⇒ InvalidField (Auto t)

else instance InvalidField t

class ToParameters record (list ∷ RowList Type) where
      toParameters ∷ Proxy list → Record record → Array (Tuple String Foreign)

instance ToParameters record Nil where
      toParameters _ _ = []

instance
      ( IsSymbol name
      , ToValue t
      , Cons name t e record
      , ToParameters record rest
      ) ⇒
      ToParameters record (Cons name t rest) where
      toParameters _ record = (DS.reflectSymbol name /\ toValue (R.get name record)) : toParameters (Proxy ∷ Proxy rest) record
            where
            name = Proxy ∷ Proxy name

-- | Simplify append qualifiying column names
class AppendPath (alias ∷ Symbol) (name ∷ Symbol) (fullPath ∷ Symbol) | alias name → fullPath

instance (Append alias Dot path, Append path name fullPath) ⇒ AppendPath alias name fullPath
