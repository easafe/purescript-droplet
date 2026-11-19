-- | Definition of SQL columns types as well conversions from and to columns
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Definition
      ( Empty
      , Identity
      , Default(..)
      , Star(..)
      , Table(..)
      , E(..)
      , Dot
      , Composite
      , Path
      , ForeignKey
      , PrimaryKey
      , Constraint
      , Unique
      , Column(..)
      , C
      , class ToType
      , class IsNullable
      , class UnwrapNullable
      , class FromValue
      , class ToParameters
      , class ToValue
      , class UnwrapDefinition
      , class AppendPath
      , class ColumnInScope
      , star
      , col
      , columns
      , qualified
      , class ColumnsOf
      , columnsOf
      , class QualifiedColumnsOf
      , qualifiedColumnsOf
      , toType
      , toParameters
      , fromValue
      , toValue
      , path
      , (...)
      ) where

import Prelude
import Prim hiding (Constraint)

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
import Data.Reflectable (class Reflectable)
import Data.Reflectable as DR
import Data.String (Pattern(..))
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Token (bigIntegerType, booleanType, dateTimeType, dateType, dotSymbol, integerType, numberType, stringType)
import Foreign (Foreign)
import Foreign as F
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, Beside, QuoteLabel, Text)
import Record as R
import Record.Builder (Builder)
import Record.Builder as RB
import Type.Proxy (Proxy(..))

foreign import readInt ∷ Foreign → Nullable Int
foreign import showForeigner ∷ Foreign → String

-- | Marks the query end
data E = E

data C ∷ ∀ k. k → Type → Type
data C n t

type Empty = ""

type Dot = "."

data Composite (name ∷ Symbol)

data Star = Star

star ∷ Star
star = Star

-- | GENERATED ALWAYS AS IDENTITY constraint
data Identity

data Default = Default

data PrimaryKey

data Unique

data ForeignKey (field ∷ Symbol) (table ∷ Type)

data Constraint ∷ ∀ n. n → Type → Type
data Constraint name t

data Column (t ∷ Type) (constraint ∷ Type) = Column

data Table (name ∷ Symbol) (fields ∷ Row Type) = Table

-- | Qualified columns (e.g, table.column)
data Path (alias ∷ Symbol) (field ∷ Symbol) = Path

path ∷ ∀ alias field path pathField. Append alias Dot path ⇒ Append path field pathField ⇒ Proxy alias → Proxy field → Path alias field
path _ _ = Path

infix 7 path as ...

-- | A column name, e.g. `col @"name"`. Same as `Proxy ∷ Proxy "name"`
col ∷ ∀ @name. Proxy name
col = Proxy

-- | A record of column names for each column of a table
-- |
-- | ```purescript
-- | u = columns users
-- | query = select (u.id /\ u.name) # from users -- SELECT "id", "name" FROM "users"
-- | ```
columns ∷ ∀ name fields list record. RowToList fields list ⇒ ColumnsOf list () record ⇒ Table name fields → Record record
columns _ = RB.buildFromScratch (columnsOf (Proxy ∷ _ list))

class ColumnsOf (list ∷ RowList Type) (from ∷ Row Type) (to ∷ Row Type) | list from → to where
      columnsOf ∷ Proxy list → Builder (Record from) (Record to)

instance ColumnsOf Nil record record where
      columnsOf _ = identity

instance (IsSymbol name, ColumnsOf rest from some, Lacks name some, Cons name (Proxy name) some to) ⇒ ColumnsOf (Cons name t rest) from to where
      columnsOf _ = RB.insert (Proxy ∷ _ name) (Proxy ∷ _ name) <<< columnsOf (Proxy ∷ _ rest)

-- | A record of qualified column names (alias.column) for each column of a table
-- |
-- | ```purescript
-- | u = qualified @"u" users
-- | query = select u.name # from (users # as @"u") -- SELECT "u"."name" "u.name" FROM "users" AS "u"
-- | ```
qualified ∷ ∀ @alias name fields list record. RowToList fields list ⇒ QualifiedColumnsOf alias list () record ⇒ Table name fields → Record record
qualified _ = RB.buildFromScratch (qualifiedColumnsOf (Proxy ∷ _ alias) (Proxy ∷ _ list))

class QualifiedColumnsOf (alias ∷ Symbol) (list ∷ RowList Type) (from ∷ Row Type) (to ∷ Row Type) | alias list from → to where
      qualifiedColumnsOf ∷ Proxy alias → Proxy list → Builder (Record from) (Record to)

instance QualifiedColumnsOf alias Nil record record where
      qualifiedColumnsOf _ _ = identity

instance (IsSymbol name, QualifiedColumnsOf alias rest from some, Lacks name some, Cons name (Path alias name) some to) ⇒ QualifiedColumnsOf alias (Cons name t rest) from to where
      qualifiedColumnsOf alias _ = RB.insert (Proxy ∷ _ name) (Path ∷ Path alias name) <<< qualifiedColumnsOf alias (Proxy ∷ _ rest)

-- | Converts a PureScript value into Postgres
class ToValue v where
      toValue ∷ v → Foreign

instance ToValue Int where
      toValue = F.unsafeToForeign

instance ToValue String where
      toValue = F.unsafeToForeign

instance ToValue Default where
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

-- | Converts a Postgres value into PureScript
class FromValue t where
      fromValue ∷ Foreign → Either String t

--sometimes node pg returns a string for integers
-- this might arise out a invalid type definition on the users part;
-- the number is actually a big int;
-- something funky
--in the first two cases, readInt returns null, as well in the latter if the string can't be parsed as an integer
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

instance UnwrapDefinition (Column t c) t

else instance UnwrapDefinition t t

-- | Convenience to remove nullable wrappers
class UnwrapNullable (w ∷ Type) (t ∷ Type) | w → t

instance UnwrapNullable (Maybe t) t

else instance UnwrapNullable t t

class IsNullable (t ∷ Type)

instance IsNullable (Maybe t)

class ToParameters record (list ∷ RowList Type) where
      toParameters ∷ Proxy list → Record record → Array (Tuple String Foreign)

instance ToParameters record Nil where
      toParameters _ _ = []

instance
      ( IsSymbol name
      , Reflectable name String
      , ToValue t
      , Cons name t e record
      , ToParameters record rest
      ) ⇒
      ToParameters record (Cons name t rest) where
      toParameters _ record = (DR.reflectType name /\ toValue (R.get name record)) : toParameters (Proxy ∷ Proxy rest) record
            where
            name = Proxy ∷ Proxy name

-- | Type of a column in scope, with a clearer error than a row mismatch when it is not
class ColumnInScope (name ∷ Symbol) (list ∷ RowList Type) (t ∷ Type) | name list → t

instance ColumnInScope name (Cons name t rest) t

else instance ColumnInScope name rest t ⇒ ColumnInScope name (Cons other u rest) t

else instance
      Fail (Beside (Text "Column ") (Beside (QuoteLabel name) (Text " is not in scope. Columns that exist on both sides of a JOIN must be qualified with their table name or alias, and tables joined more than once must be aliased"))) ⇒
      ColumnInScope name Nil t

-- | Simplify append qualifying column names
class AppendPath (alias ∷ Symbol) (name ∷ Symbol) (fullPath ∷ Symbol) | alias name → fullPath

instance (Append alias Dot path, Append path name fullPath) ⇒ AppendPath alias name fullPath

-- | How a value should be generated for DEFAULT and other constraints
-- |
-- | Required only if using migrations; other cases are handled by `ToValue`
class ToConstraintValue (t ∷ Type) where
      toConstraintValue ∷ Proxy t → Foreign

-- | String representation of field types
class ToType (t ∷ Type) where
      toType ∷ Proxy t → String

instance ToType Int where
      toType _ = integerType

instance ToType BigInt where
      toType _ = bigIntegerType

instance ToType Date where
      toType _ = dateType

instance ToType DateTime where
      toType _ = dateTimeType

instance ToType String where
      toType _ = stringType

instance ToType Number where
      toType _ = numberType

instance ToType Boolean where
      toType _ = booleanType

instance ToType t ⇒ ToType (Maybe t) where
      toType _ = toType (Proxy ∷ _ t)