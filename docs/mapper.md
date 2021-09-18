---
layout: default
title: Query mapper
---

## Querying the database

Essentially, the query mapper runs a given query and converts its output into records. The first step is to create a connection pool

```haskell
newPool :: Configuration -> Effect Pool
```

and use one of either

```haskell
withConnection :: forall a. Pool -> (Either PgError Connection -> Aff a) -> Aff a

withTransaction :: forall a. Pool -> (Connection -> Aff a) -> Aff (Either PgError a)
```

to obtain a connection. The output of a query always matches the `projection` of a SELECT or RETURNING statement; otherwise, queries with no output (e.g., DELETE or UPDATE) yield `Unit`. The primary querying functions to access the database are

```haskell
query ::
      forall q projection pro.
      ToQuery q projection =>
      RowToList projection pro =>
      FromResult pro (Record projection) =>
      Connection ->
      q ->
      Aff (Either PgError (Array (Record projection)))

execute :: forall q. ToQuery q () => Connection -> q -> Aff (Maybe PgError)

single ::
      forall q projection pro.
      ToQuery q projection =>
      RowToList projection pro =>
      FromResult pro (Record projection) =>
      Connection ->
      q ->
      Aff (Either PgError (Maybe (Record projection)))
```

`query` fetches an arbitrary number of results. `execute` is intended for queries without an output. `single` is for queries with exactly zero or one results.

Because queries can be freely composed, a few checks can only be performed when using one of the querying functions above. This includes checking for out of scope column access, ill formed aggregations, invalid top levels, etc.

For the mapping to work, all columns names in a projection must be unique -- same reason as why literals and functions must be aliased. Since names are always quote by the query mapper, it is safe to use any casing in columns or aliases. This also means table aliases are not stripped. For example, running the query `SELECT u.id FROM users AS u` on Postgres renders columns as "id"; the Droplet equivalent `select (u ... id) # from (users # as u)` results in the record `{ "u.id" :: Int }`.

## Unsafe queries

For each querying function, there is an unsafe counterpart that accepts raw SQL strings

```haskell
unsafeQuery ::
      forall projection pro parameters pra.
      RowToList projection pro =>
      RowToList parameters pra =>
      ToParameters parameters pra =>
      FromResult pro (Record projection) =>
      Connection ->
      Maybe Plan ->
      String ->
      Record parameters ->
      Aff (Either PgError (Array (Record projection)))

unsafeExecute ::
      forall parameters pra.
      RowToList parameters pra =>
      ToParameters parameters pra =>
      Connection ->
      Maybe Plan ->
      String ->
      Record parameters ->
      Aff (Maybe PgError)

unsafeSingle ::
      forall parameters pra projection pro.
      RowToList parameters pra =>
      ToParameters parameters pra =>
      RowToList projection pro =>
      FromResult pro (Record projection) =>
      Connection ->
      Maybe Plan ->
      String ->
      Record parameters ->
      Aff (Either PgError (Maybe (Record projection)))
```

A type annotation will likely be required to determine the output records. The `Maybe Plan` parameter is for prepared statements. To add parameters to your query, use the notation `@name` and include a matching key in the `Record parameters` argument. For example

```haskell
selectUnsafe :: Aff (Either PgError (Array {id :: Int}))
selectUnsafe = withConnection pool $ \c -> do
      ...
      unsafeQuery connection Nothing "SELECT name FROM users WHERE id = @id" { id : 34 }

selectUnsafe :: Aff (Maybe PgError)
selectUnsafe = withConnection pool $ \c -> do
      ...
      unsafeExecute connection Nothing "INSERT INTO users (name) VALUEs (@name)" { name : "mary" }
```

## Type mapping

Most common types (integers, strings, date/datetime, etc.) work out of the box. In case the default behavior isn't desirable, or you need to map columns to custom types, the type classes `FromValue` and `ToValue` can perform conversions. `FromValue` translates `Foreign` SQL values to PureScript; `ToValue` parses PureScript values into `Foreign`. All parameters must also have `ToValue` instances.

<a href="/edsl" class="direction previous">Previous: eDSL</a>
