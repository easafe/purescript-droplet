---
layout: default
title: Query mapper
---

Essentially, the query mapper runs a given query and converts its output into records.

## Querying the database

The output always the `projection` of a SELECT or RETURNING and it is the unit type for queries without an output (e.g., DELETE or UPDATE). The primary querying functions to access the database are

```haskell
query ∷
      ∀ q projection pro.
      ToQuery q projection ⇒
      RowToList projection pro ⇒
      FromResult pro (Record projection) ⇒
      Connection →
      q →
      Aff (Either PgError (Array (Record projection)))

execute ∷ ∀ q. ToQuery q () ⇒ Connection → q → Aff (Maybe PgError)

single ∷
      ∀ q projection pro.
      ToQuery q projection ⇒
      RowToList projection pro ⇒
      FromResult pro (Record projection) ⇒
      Connection →
      q →
      Aff (Either PgError (Maybe (Record projection)))
```

`query` fetches many results. `execute` is intended for queries without an output. `single` is for queries with exactly zero or one results. A connection can be obtained from `withConnection` or `withTransaction`, after a connection pool has been created

```haskell
newPool ∷ Configuration → Effect Pool

withConnection ∷ ∀ a. Pool → (Either PgError Connection → Aff a) → Aff a

withTransaction ∷ ∀ a. Pool → (Connection → Aff a) → Aff (Either PgError a)
```

Because queries can be freely composed, a few checks can only be performed when using one of the querying functions. This includes checking for out of scope column access, ill formed aggregations, invalid top levels, etc. For the mapping to work, all columns names in a projection must be unique; for the same reason., literals and functions must be aliased.

## Unsafe queries

For each querying function, there is an unsafe counterpart that allows raw sql strings

```haskell
unsafeQuery ∷
      ∀ projection pro parameters pra.
      RowToList projection pro ⇒
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      FromResult pro (Record projection) ⇒
      Connection →
      Maybe Plan →
      String →
      Record parameters →
      Aff (Either PgError (Array (Record projection)))

unsafeExecute ∷
      ∀ parameters pra.
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      Connection →
      Maybe Plan →
      String →
      Record parameters →
      Aff (Maybe PgError)

unsafeSingle ∷
      ∀ parameters pra projection pro.
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      RowToList projection pro ⇒
      FromResult pro (Record projection) ⇒
      Connection →
      Maybe Plan →
      String →
      Record parameters →
      Aff (Either PgError (Maybe (Record projection)))
```

A type annotation will likely be required to determine the output records. The `Maybe Plan` parameter is for prepared statements. To use parameters in your query use the notation `@name` and include in the `Record parameters` argument, for example

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

Most common types (integers, strings, date/datetime, etc.) work out of the box. In case the default behavior isn't desirable, or you need to map columns to some custom type, the type classes `FromValue` and `ToValue` can be instantiated. `FromValue` translates SQL values to PureScript; `ToValue` parses PureScript values into `Foreign` used by Postgres. All parameters must also have `ToValue` instances.

<a href="/edsl" class="direction previous">Previous: eDSL</a>
