module Droplet.Internal.Mapper.Driver where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (head, (:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null, toMaybe, toNullable)
import Data.Nullable as DN
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Droplet.Internal.Edsl.Filter (NotParameterized)
import Droplet.Internal.Edsl.Language (Plan(..))
import Droplet.Internal.Mapper.Pool (Pool)
import Droplet.Internal.Mapper.Query (Query(..))
import Droplet.Internal.Mapper.Query as DIMQ
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (Foreign)
import Foreign as F
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record (delete) as Record
import Record as R
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

type ConnectResult = {
      client :: Client,
      done :: Effect Unit
}

data PGError =
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

-- | Those instances are required for testing.
instance eqPGError :: Eq PGError where
      eq = case _, _ of
            (ClientError _ s1), (ClientError _ s2) -> s1 == s2
            (ConversionError s1), (ConversionError s2) -> s1 == s2
            (InternalError err1), (InternalError err2) -> eqErr err1 err2
            (OperationalError err1), (OperationalError err2) -> eqErr err1 err2
            (ProgrammingError err1), (ProgrammingError err2) -> eqErr err1 err2
            (IntegrityError err1), (IntegrityError err2) -> eqErr err1 err2
            (DataError err1), (DataError err2) -> eqErr err1 err2
            (NotSupportedError err1), (NotSupportedError err2) -> eqErr err1 err2
            (QueryCanceledError err1), (QueryCanceledError err2) -> eqErr err1 err2
            (TransactionRollbackError err1), (TransactionRollbackError err2) -> eqErr err1 err2
            _, _ -> false
            where eqErr err1 err2 =
                        let _error = Proxy :: Proxy "error" in eq (Record.delete _error err1) (Record.delete _error err2)

derive instance genericPGError :: Generic PGError _

instance showPGError :: Show PGError where
      show = genericShow

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
derive instance newtypeConnection :: Newtype Connection _

-- | APIs of the `Pool.query` and `Client.query` are the same.
-- | We can dse this polyformphis to simplify ffi.
foreign import data UntaggedConnection :: Type

-- | PostgreSQL connection.
foreign import data Client :: Type

foreign import connect_ :: forall a. {
      nullableLeft :: Error -> Nullable (Either PGError ConnectResult),
      right :: a -> Either PGError ConnectResult
} -> Pool -> EffectFnAff (Either PGError ConnectResult)

type QueryResult r = {
      rows :: Homogeneous r Foreign => Array (Record r),
      rowCount :: Int
}

type RawQuery = {
      name :: Nullable String,
      text :: String,
      values :: Array Foreign
}

foreign import rawQuery_ :: forall r. {
      nullableLeft :: Error -> Nullable (Either PGError (QueryResult r)),
      right :: QueryResult r -> Either PGError (QueryResult r)
} -> UntaggedConnection -> RawQuery -> EffectFnAff (Either PGError (QueryResult r))

foreign import sqlState_ :: Error -> Nullable String
foreign import errorDetail_ :: Error -> PGErrorDetail

class ToValues record (list :: RowList Type) where
      toValues :: Proxy list -> Record record -> Array Foreign

instance nilToValues :: ToValues record RL.Nil where
      toValues _ _ = []

instance consToValues :: (IsSymbol name, Cons name t e record, ToValues record rest) => ToValues record (RL.Cons name t rest) where
      toValues _ record = F.unsafeToForeign (R.get (Proxy :: Proxy name) record) : toValues (Proxy :: Proxy rest) record

--Query needs to include the final output in its type
-- select columns need a way to map fields to whatever type (only for outermost select tho!)
-- we prolly need something that formats parameters
-- query :: forall q projection parameters result. Query q projection starting => ToResult projection result Connection -> q -> Aff (Either PGError result)
-- query connection q = do
--       raw <- rawQuery connection $ DIMQ.query q
--       pure $ raw >>= _.rows >>> traverse (?tr >>> lmap ConversionError)

-- r has to match fields of projection
-- rawQuery :: forall projection r parameters list. RowToList parameters list => ToValues parameters list => Connection -> Query projection parameters -> Aff (Either PGError (QueryResult r))
-- rawQuery connection q = EAC.fromEffectFnAff $ rawQuery_ rightLeft (toUntaggedHandler connection) rq
--       where rq = case q of
--                   Plain s -> {
--                         name: null,
--                         text: s,
--                         values: []
--                   }
--                   Parameterized plan _ s p -> {
--                         name: case plan of
--                               NotNamed -> null
--                               Named n -> DN.notNull n,
--                         text: s,
--                         values: toValues (Proxy :: Proxy list) p
--                   }

--             toUntaggedHandler :: Connection -> UntaggedConnection
--             toUntaggedHandler (Connection c) = case c of
--                   Left pool -> unsafeCoerce pool
--                   Right client -> unsafeCoerce client

--             rightLeft = {
--                   nullableLeft: toNullable <<< map Left <<< convertError,
--                   right: Right
--             }

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withClient :: forall a. Pool -> (Either PGError Client -> Aff a) -> Aff a
withClient p k = bracket (connect p) cleanup run
      where cleanup = case _ of
                  Left _ -> pure unit
                  Right { done } -> liftEffect done

            run = case _ of
                  Left err -> k $ Left err
                  Right { client } -> k $ Right client

connect :: Pool -> Aff (Either PGError ConnectResult)
connect = EAC.fromEffectFnAff <<< connect_ {
      nullableLeft: toNullable <<< map Left <<< convertError,
      right: Right
}

-- | Trivial helper / shortcut which also wraps
-- | the connection to provide `Connection`.
withConnection :: forall a. Pool -> (Either PGError Connection -> Aff a) -> Aff a
withConnection p k = withClient p (lcmap (map fromClient) k)

-- | TODO: Provide docs
-- withTransaction :: forall a. Pool -> (Connection -> Aff a) -> Aff (Either PGError a)
-- withTransaction pool action =
--       withClient pool case _ of
--             Right client ->
--                   withClientTransaction client do
--                         (action $ fromClient client)
--             Left err -> pure $ Left err

-- -- | TODO: Outdated docs
-- -- | Run an action within a transaction. The transaction is committed if the
-- -- | action returns cleanly, and rolled back if the action throws (either a
-- -- | `PGError` or a JavaScript exception in the Aff context). If you want to
-- -- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- -- | within the transaction.
-- withClientTransaction :: forall a. Client -> Aff a -> Aff (Either PGError a)
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

fromPool :: Pool -> Connection
fromPool pool = Connection (Left pool)

fromClient :: Client -> Connection
fromClient client = Connection (Right client)

-- -- | Execute a PostgreSQL query and discard its results.
-- execute ::
--       forall i o.
--       (ToSQLRow i) =>
--       Connection ->
--       Query i o ->
--       i ->
--       Aff (Maybe PGError)
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
--       Aff (Either PGError (Maybe o))
-- scalar conn sql values = query conn sql values <#> map (head >>> map (case _ of Row1 a -> a))

convertError :: Error -> Maybe PGError
convertError err = case toMaybe $ sqlState_ err of
      Nothing -> Nothing
      Just sqlState -> Just $ convert sqlState $ errorDetail_ err
      where convert :: String -> PGErrorDetail -> PGError
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
