---
layout: default
title: eDSL
---

## Introduction

Before looking at the eDSL (embedded Domain Specific Language) syntax, let's talk about its types

* Database types

In order to write queries, we have to define types for database objects. As an example, the schema defined in [getting started](/index) could be represented as

```haskell
-- `users` table
type Users = (
    id :: Auto Int,
    name :: String,
    birthday :: Maybe Date
)

users :: Table "users" Users
users = Table

-- `messages` table
type Messages = (
    id :: Auto Int,
    sender :: Int,
    recipient :: Int,
    date :: Default DateTime
)

messages :: Table "messages" Messages
messages = Table
```

`Users` refers to the table columns, whereas `users` ties the columns with the table name. `Auto` and `Default` refer to identity and default columns. Expectedly, `Maybe` represent nullable columns.

Since the columns are at type level, we need `Proxy`s to represent their names

```haskell
id :: Proxy "id"
id = Proxy

name :: Proxy "name"
name = Proxy

date :: Proxy "date"
date = Proxy

birthday :: Proxy "birthday"
birthday = Proxy

sender :: Proxy "sender"
sender = Proxy

recipient :: Proxy "recipient"
recipient = Proxy
```

and aliases, as well

```haskell
m :: Proxy "m"
m = Proxy

u :: Proxy "u"
u = Proxy

t :: Proxy "t"
t = Proxy
```

* Query types

The eDSL is designed to be composable, and resemble SQL syntax as much as possible so the generated code is a direct equivalent. For those reasons, function types are somewhat opaque. On the other hand, data types (such as for SELECT, WHERE or JOIN) can become quite verbose quite fast. Luckily, when writing queries, type annotations are optional: the type checker should be able to infer the type of valid queries without any hints. That being said, specially when debugging type errors in your queries, it might be useful to know some of the conventions employed in the eDSL.

```haskell
-- select :: forall s projection. ToSelect s => s -> Select s projection E
-- from :: forall f q fields sql. ToFrom f q fields => Resume q (From f fields E) sql => f -> q -> sql
-- wher :: forall c q sql. ToWhere c q => Resume q (Where c E) sql => c -> q -> sql

exampleQuery :: Select (Proxy "name") (name :: String) (From (Table "users" Users) Users (Where (Op (Proxy "id") Int) E))
exampleQuery = select name # from users # wher (id .=. 9)
```

It is not important to immediately understand all the types. The main draws are:

1. Left association

To aid composition, functions are designed to be take the current query as their last parameter. While it is possible to write `wher (id .=. 9) (from users (select name))`, we will stick with `#` in this guide

2. `To` type classes

Type classes in the form `ToFrom`, `ToWhere` etc, are used to tell which statements are allowed in sequence (e.g., FROM can only follow after SELECT or DELETE, etc). These type classes have no function members

3. `Resume` type class

eDSL functions mark the end of a statement with the `E` data type. The type class `Resume` replaces it with a further statement, for example, `Select s projection E` => `Select s projection (From f fields E)`


## SELECT

SELECT is typed as

```haskell
select :: forall s projection. ToSelect s => s -> Select s projection E
```

### Projections

`select` can project columns, literals, subqueries and functions and star.

```haskell
selectColumn :: forall projection. Select (Proxy "id") projection E
selectColumn = select id

-- literals must be aliased
selectLiteral :: forall projection. Select (As "m" Int) projection E
selectLiteral = select (3 # as m)

selectStar :: forall projection. Select Star projection E
selectStar = select star

-- same as SELECT table alias.column name
selectQualifiedColumn :: forall projection. Select (Path "u" "id") projection E
selectQualifiedColumn = select (u ... id)

-- functions must be aliased
selectCount :: forall projection fields. Select (As "u" (Aggregate Star E fields BigInt)) projection E
selectCount = select (count star # as u)

selectSubQuery :: _
selectSubQuery = select (select name # from users # wher (u ... id .=. id) # orderBy id # limit 1) # from (messages # as u)

selectManyColumns :: forall projection. Select (Tuple (Proxy "id") (Tuple (Proxy "name") (Tuple (As "u" Int) (Path "m" "id")))) projection E
selectManyColumns = select (id /\ name /\ (5 # as u) /\ m ... id)
```

In the case of fully formed SELECT statements, `projection` becomes a `Row Type` of the output

```haskell
exampleProjection :: Select (Tuple (Proxy "id") (Proxy "name")) (id :: Int, name :: String) _
exampleProjection = select (id /\ name) # from users
```

Note that `select` on its own accepts any column name. Queries are checked only after FROM is used.

### Subqueries

Subqueries must return a single column. Columns from outer scopes can be referenced with `(alias ... column)`.

```haskell
subQueryExample :: _
subQueryExample = select (select name # from users # wher (u ... id .=. id) # orderBy id # limit 1) # from (messages # as u)
```

### Functions



### DISTINCT

```haskell
distinct :: forall s. ToSelect s => s -> Distinct s
```

DISTINCT is subject the same rules as SELECT.

```haskell
selectDistinctColumn :: Select (Distinct (Proxy "id")) (id :: Int) _
selectDistinctColumn = select (distinct id) # from users

selectDistinctColumns :: Select (Distinct (Tuple (Proxy "id") (Tuple (Proxy "name") (Proxy "birthday")))) (birthday :: Maybe Date, id :: Int, name :: String) _
selectDistinctColumns = select (distinct $ id /\ name /\ birthday) # from users
```


### FROM

FROM statement keeps track of columns in scope. For this reason, its type is a bit more complex than SELECT

```haskell
from :: forall f q fields sql. ToFrom f q fields => Resume q (From f fields E) sql => f -> q -> sql
```

The type parameter `f`, indicating the source of columns, can be a

* Table

We can select tables as they are

```haskell
fromTable :: Select (Proxy "id") (id :: Int) _
fromTable = select id # from users
```

or with a alias

```haskell
fromTableAlias :: Select (Proxy "id") (id :: Int) _
fromTableAlias = select id # from (users # as u) -- SELECT id FROM users AS u

fromTableAlias2 :: Select (Path "u" "id") ("u.id" :: Int) _
fromTableAlias2 = select (u ... id) # from (users # as u) -- SELECT u.id FROM users AS u
```

* Sub query

Subqueries in FROM must have an alias

```haskell
fromSubQuery :: Select (Proxy "name") (name :: String) _
fromSubQuery = select name # from (select star # from users # as u) -- SELECT name FROM (SELECT * FROM users) AS u
```

* Join

To be parsed correctly, joins must be bracketed into FROM. Joined expressions can any valid FROM expression, that is, tables, sub queries, other joins, etc. Currently, a following ON clause is mandatory.

1. INNER JOIN

Returns a cartesian product of both expressions

```haskell
queryInnerJoin :: Select (Path "u" "name") ("u.name" :: String) _
queryInnerJoin = select ( u ... name) # from ((messages # as m) `join` (users # as u) # on (m ... sender .=. u ... id)) -- SELECT u.name FROM messages AS m INNER JOIN users AS u ON m.sender = u.id

queryInnerJoin2 :: Select (Tuple (Path "u" "name") (Path "m" "sender")) ("m.sender" :: Int, "u.name" :: String) _
queryInnerJoin2 = select ( u ... name /\ m ... sender) # from ((select sender # from messages # as m) `join` (users # as u) # on (m ... sender .=. u ... id)) -- SELECT u.name, m.sender FROM (SELECT "sender" FROM messages) AS m INNER JOIN users AS u ON m.sender = u.id
```

2. LEFT OUTER JOIN

Returns a cartesian product of both expressions plus each row in the left hand expression that had no match on the right side. Right side columns will become `Maybe` in the projection type.

```haskell
queryOuterJoin :: Select (Tuple (Path "u" "name") (Path "m" "sender")) ("m.sender" :: Int, "u.name" :: Maybe String) _
queryOuterJoin = select (u ... name /\ m ... sender) # from ((messages # as m) `leftJoin` (users # as u) # on (m ... sender .=. u ... id)) -- SELECT u.name, m.sender FROM messages AS m OUTER JOIN users AS u ON m.sender = u.id
```

### GROUP BY

### ORDER BY

### LIMIT

### OFFSET

### UNION

## WHERE

## INSERT

## UPDATE

## DELETE

## RETURNING

## AS

## PREPARE


<a href="/index" class="direction previous">Previous: Getting started</a>
<a href="/mapper" class="direction">Next: Query Mapper</a>
