module Droplet.Internal.Mapper.Driver where

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
import Data.Generic.Rep (class Generic)
import Data.Int as DI
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null, toMaybe, toNullable)
import Data.Nullable as DN
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as DST
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Traversable as DT
import Droplet (NotParameterized)
import Droplet.Internal.Edsl.Language (Plan(..))
import Droplet.Internal.Mapper.Pool (Pool)
import Droplet.Internal.Mapper.Query (class ToQuery, Query(..))
import Droplet.Internal.Mapper.Query as DIMQ
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (Foreign)
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Partial.Unsafe as PU
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record (delete) as Record
import Record as R
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type ConnectResult = {
      client :: Client,
      done :: Effect Unit
}

data PgError =
      ClientError Error String |
      ConversionError String |
      InternalError PGErrorDetail |
      OperationalError PGErrorDetail |
      ProgrammingError PGErrorDetail |
      IntegrityError PGErrorDetail |
      DataError PGErrorDetail |
      NotSupportedError PGErrorDetail |
      QueryCanceledError PGErrorDetail |
      TransactionRollbackError PGErrorDetail

type PGErrorDetail = {
      severity :: String,
      code :: String,
      message :: String,
      detail :: String,
      error :: Error,
      hint :: String,
      position :: String,
      internalPosition :: String,
      internalQuery :: String,
      where_ :: String,
      schema :: String,
      table :: String,
      column :: String,
      dataType :: String,
      constraint :: String,
      file :: String,
      line :: String,
      routine :: String
}

newtype Connection = Connection (Either Pool Client)

-- | APIs of the `Pool.query` and `Client.query` are the same.
-- | We can dse this polyformphis to simplify ffi.
foreign import data UntaggedConnection :: Type

-- | PostgreSQL connection.
foreign import data Client :: Type

type RawQuery = {
      name :: Nullable String,
      text :: String,
      values :: Array Foreign
}

-- | Those instances are required for testing.
instance eqPGError :: Eq PgError where
      eq = case _, _ of
            ClientError _ s1, ClientError _ s2 -> s1 == s2
            ConversionError s1, ConversionError s2 -> s1 == s2
            InternalError err1, InternalError err2 -> eqErr err1 err2
            OperationalError err1, OperationalError err2 -> eqErr err1 err2
            ProgrammingError err1, ProgrammingError err2 -> eqErr err1 err2
            IntegrityError err1, IntegrityError err2 -> eqErr err1 err2
            DataError err1, DataError err2 -> eqErr err1 err2
            NotSupportedError err1, NotSupportedError err2 -> eqErr err1 err2
            QueryCanceledError err1, QueryCanceledError err2 -> eqErr err1 err2
            TransactionRollbackError err1, TransactionRollbackError err2 -> eqErr err1 err2
            _, _ -> false
            where eqErr err1 err2 =
                        let _error = Proxy :: Proxy "error" in eq (Record.delete _error err1) (Record.delete _error err2)
derive instance genericPGError :: Generic PgError _
derive instance newtypeConnection :: Newtype Connection _
instance showPGError :: Show PgError where
      show = genericShow




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



class ToParameters record (list :: RowList Type) where
      toValues :: Proxy list -> Record record -> Array Foreign

instance nilToParameters :: ToParameters record RL.Nil where
      toValues _ _ = []

instance consToParameters :: (
      IsSymbol name,
      ToValue t,
      Cons name t e record,
      ToParameters record rest
) => ToParameters record (RL.Cons name t rest) where
      toValues _ record = toValue (R.get (Proxy :: Proxy name) record) : toValues (Proxy :: Proxy rest) record



class FromResult (projection :: RowList Type) result | projection -> result where
      toResult :: Proxy projection -> Object Foreign -> Either String result

instance nilFromResult :: FromResult RL.Nil (Record ()) where
      toResult _ _ = Right {}

instance consFromResult :: (
      FromValue t,
      FromResult rest (Record restProjection),
      IsSymbol name,
      Lacks name restProjection,
      Cons name t restProjection projection
) => FromResult (RL.Cons name t rest) (Record projection) where
      toResult _ raw = case fromValue value of
            Left error -> Left error
            Right converted -> map (R.insert (Proxy :: Proxy name) converted) $ toResult (Proxy :: Proxy rest) raw
            where value = PU.unsafePartial (DM.fromJust $ FO.lookup (DS.reflectSymbol (Proxy :: Proxy name)) raw)



foreign import connect_ :: forall a. {
      nullableLeft :: Error -> Nullable (Either PgError ConnectResult),
      right :: a -> Either PgError ConnectResult
} -> Pool -> EffectFnAff (Either PgError ConnectResult)

foreign import rawQuery_ :: {
      nullableLeft :: Error -> Nullable (Either PgError (Array (Object Foreign))),
      right :: Array (Object Foreign) -> Either PgError (Array (Object Foreign))
} -> UntaggedConnection -> RawQuery -> EffectFnAff (Either PgError (Array (Object Foreign)))

foreign import sqlState_ :: Error -> Nullable String
foreign import errorDetail_ :: Error -> PGErrorDetail



-------------------------------CONNECTING----------------------------------------------

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withClient :: forall a. Pool -> (Either PgError Client -> Aff a) -> Aff a
withClient p k = bracket (connect p) cleanup run
      where cleanup = case _ of
                  Left _ -> pure unit
                  Right { done } -> liftEffect done

            run = case _ of
                  Left err -> k $ Left err
                  Right { client } -> k $ Right client

connect :: Pool -> Aff (Either PgError ConnectResult)
connect = EAC.fromEffectFnAff <<< connect_ rightLeft

-- | Trivial helper / shortcut which also wraps
-- | the connection to provide `Connection`.
withConnection :: forall a. Pool -> (Either PgError Connection -> Aff a) -> Aff a
withConnection p k = withClient p (lcmap (map fromClient) k)

-- | TODO: Provide docs
-- withTransaction :: forall a. Pool -> (Connection -> Aff a) -> Aff (Either PgError a)
-- withTransaction pool action =
--       withClient pool case _ of
--             Right client ->
--                   withClientTransaction client do
--                         (action $ fromClient client)
--             Left err -> pure $ Left err

-- -- | TODO: Outdated docs
-- -- | Run an action within a transaction. The transaction is committed if the
-- -- | action returns cleanly, and rolled back if the action throws (either a
-- -- | `PgError` or a JavaScript exception in the Aff context). If you want to
-- -- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- -- | within the transaction.
-- withClientTransaction :: forall a. Client -> Aff a -> Aff (Either PgError a)
-- withClientTransaction client action =
--       begin >>= case _ of
--                   Nothing -> do
--                         a <- action `catchError` \jsErr -> do
--                               void $ rollback
--                               throwError jsErr
--                         commit >>= case _ of
--                               Just pgError -> pure (Left pgError)
--                               Nothing -> pure (Right a)
--                   Just pgError -> pure (Left pgError)
--       where conn = fromClient client
--             begin = execute conn (Query "BEGIN TRANSACTION") Row0
--             commit = execute conn (Query "COMMIT TRANSACTION") Row0
--             rollback = execute conn (Query "ROLLBACK TRANSACTION") Row0


-------------------------------QUERYING----------------------------------

type Has = NotParameterized

-- select columns need a way to map fields to whatever type (only for outermost select tho!)
query :: forall q projection parameters pro par.
      RowToList parameters par =>
      ToParameters parameters par =>
      ToQuery q projection Has parameters =>
      RowToList projection pro =>
      FromResult pro (Record projection) =>
      Connection ->
      q ->
      Aff (Either PgError (Array (Record projection)))
query connection q = do
      raw <- rawResults
      case raw of
            Left error -> pure $ Left error
            Right r -> pure $ DT.traverse (DB.lmap ConversionError <<< toResult (Proxy :: Proxy pro)) r
      where rawResults :: Aff (Either PgError (Array (Object Foreign)))
            rawResults = EAC.fromEffectFnAff <<< rawQuery_ rightLeft (toUntaggedHandler connection) $ case DIMQ.query q of
                  NotParameterized s -> {
                        name: null,
                        text: s,
                        values: []
                  }
                  Parameterized plan _ s p -> {
                        name: case plan of
                              NotNamed -> null
                              Named n -> DN.notNull n,
                        text: s,
                        values: toValues (Proxy :: Proxy par) p
                  }

--should this be made nicer?
unsafeQuery :: forall projection parameters pro par.
      RowToList parameters par =>
      ToParameters parameters par =>
      RowToList projection pro =>
      FromResult pro (Record projection) =>
      Connection ->
      Query projection parameters ->
      Aff (Either PgError (Array (Record projection)))
unsafeQuery connection q = query connection q

-- -- | Execute a PostgreSQL query and discard its results.
-- execute ::
--       forall i o.
--       (ToSQLRow i) =>
--       Connection ->
--       Query i o ->
--       i ->
--       Aff (Maybe PgError)
-- execute conn (Query sql) values = hush <<< either Right Left <$> rawQuery conn sql (toSQLRow values)

-- | Execute a PostgreSQL query and return the first field of the first row in
-- | the result.
-- scalar ::
--       forall i o.
--       ToSQLRow i =>
--       FromSQLValue o =>
--       Connection ->
--       Query i (Row1 o) ->
--       i ->
--       Aff (Either PgError (Maybe o))
-- scalar conn sql values = query conn sql values <#> map (head >>> map (case _ of Row1 a -> a))




toUntaggedHandler :: Connection -> UntaggedConnection
toUntaggedHandler (Connection c) = case c of
      Left pool -> unsafeCoerce pool
      Right client -> unsafeCoerce client

fromPool :: Pool -> Connection
fromPool pool = Connection (Left pool)

fromClient :: Client -> Connection
fromClient client = Connection (Right client)

rightLeft :: forall r s t. {
      nullableLeft :: Error -> Nullable (Either PgError r),
      right :: t -> Either s t
}
rightLeft = {
      nullableLeft: toNullable <<< map Left <<< convertError,
      right: Right
}

convertError :: Error -> Maybe PgError
convertError err = case toMaybe $ sqlState_ err of
      Nothing -> Nothing
      Just sqlState -> Just $ convert sqlState $ errorDetail_ err
      where convert :: String -> PGErrorDetail -> PgError
            convert s =
                  if prefix "0A" s then
                        NotSupportedError
                  else if prefix "20" s || prefix "21" s then
                        ProgrammingError
                  else if prefix "22" s then
                        DataError
                  else if prefix "23" s then
                        IntegrityError
                  else if prefix "24" s || prefix "25" s then
                        InternalError
                  else if prefix "26" s || prefix "27" s || prefix "28" s then
                        OperationalError
                  else if prefix "2B" s || prefix "2D" s || prefix "2F" s then
                        InternalError
                  else if prefix "34" s then
                        OperationalError
                  else if prefix "38" s || prefix "39" s || prefix "3B" s then
                        InternalError
                  else if prefix "3D" s || prefix "3F" s then
                        ProgrammingError
                  else if prefix "40" s then
                        TransactionRollbackError
                  else if prefix "42" s || prefix "44" s then
                        ProgrammingError
                  else if s == "57014" then
                        QueryCanceledError
                  else if prefix "5" s then
                        OperationalError
                  else if prefix "F" s then
                        InternalError
                  else if prefix "H" s then
                        OperationalError
                  else if prefix "P" s then
                        InternalError
                  else if prefix "X" s then
                        InternalError
                  else
                        const $ ClientError err s

            prefix :: String -> String -> Boolean
            prefix p = maybe false (_ == 0) <<< String.indexOf (Pattern p)
