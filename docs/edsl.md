---
layout: default
title: eDSL
---

## Introduction

Before looking at the eDSL (embedded Domain Specific Language) syntax, let's talk about its types

* Database types

In order to write queries, we have to define types for database objects. As an example, the schema from [getting started](/index) could be represented as

```haskell
-- `users` table
type Users = (
    id :: Column Int (PrimaryKey /\ Identity),
    name :: String,
    birthday :: Maybe Date
)
type UsersTable = Table "users" Users

users :: UsersTable
users = Table

-- `messages` table
type Messages = (
    id :: Column Int (PrimaryKey /\ Identity),
    sender :: Column Int (ForeignKey "id" UsersTable),
    recipient :: Column Int (ForeignKey "id" UsersTable),
    date :: Column DateTime Default
)

messages :: Table "messages" Messages
messages = Table
```

`Users` refers to the table columns, whereas `users` ties the columns to the table name. You can read the [migration guide](/migrations) to learn more about defining SQL type representations.

Since the columns are at the type level, we need `Proxy`s to represent their names

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
-- from :: forall f q columns sql. ToFrom f q columns => Resume q (From f columns E) sql => f -> q -> sql
-- wher :: forall c q sql. ToWhere c q => Resume q (Where c E) sql => c -> q -> sql

exampleQuery :: Select (Proxy "name") (name :: String) (From (Table "users" Users) Users (Where (Op (Proxy "id") Int) E))
exampleQuery = select name # from users # wher (id .=. 9)
```

It is not important to immediately understand all the types. The main draws are:

* Left association

To aid composition, functions are designed to be take the current query as their last parameter. While it is possible to write `wher (id .=. 9) (from users (select name))`, we will stick with `#` in this guide

* `To` type classes

Type classes like `ToFrom`, `ToWhere` etc., serve to tell which statements are allowed in sequence (e.g., FROM can only follow after SELECT or DELETE, etc). These type classes have no function members

* `Resume` type class

The end of a statement is marked by the `E` data type. The type class `Resume` can replace it with a further statement, for example, `Select s projection E` => `Select s projection (From f columns E)`

Lastly, tuples (via `/\`) stand in for commas, e.g., `select (column /\ column2 /\ columnN) ... groupBy (column /\ column2 /\ columnN) ... orderBy (column /\ column2 /\ columnN)` == `SELECT column, column2, columnN ... GROUP BY column, column2, columnN ... ORDER BY column, column2, columnN`

## SELECT

SELECT is typed as

```haskell
select :: forall s projection. ToSelect s => s -> Select s projection E
```

### Projections

`select` can project columns, literals, subqueries, functions and ```*```.

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
selectCount :: forall projection columns. Select (As "u" (Aggregate Star E columns BigInt)) projection E
selectCount = select (count star # as u)

selectSubQuery :: _
selectSubQuery = select (select name # from users # wher (u ... id .=. id) # orderBy id # limit 1) # from (messages # as u)

selectManyColumns :: forall projection. Select (Tuple (Proxy "id") (Tuple (Proxy "name") (Tuple (As "u" Int) (Path "m" "id")))) projection E
selectManyColumns = select (id /\ name /\ (5 # as u) /\ m ... id)
```

In the case of fully formed SELECT statements, `projection` becomes a `Row Type` of the output. Note that columns constraints and other type wrappers are removed. For example, selecting id from `Users` yields ```(id :: Int)``` and not ```(id :: Column Int (PrimaryKey /\ Identity))```

```haskell
exampleProjection :: Select (Tuple (Proxy "id") (Proxy "name")) (id :: Int, name :: String) _
exampleProjection = select (id /\ name) # from users
```

Note that `select` on its own accepts any column name. Queries are checked only after FROM is used.

### Subqueries

Subqueries must return a single column. Columns from outer scope can be referenced with `(alias ... column)`.

```haskell
subQueryExample :: _
subQueryExample = select (select name # from users # wher (u ... id .=. id) # orderBy id # limit 1) # from (messages # as u)
```

### Functions

Droplet offers a few functions built-in:

* `count`

* `string_agg`

* `coalesce`

* `random`

User defined (or missing) functions can be declared using `function` (or `function'`)

```haskell
-- represents a function that takes arguments
function :: forall input output. String -> FunctionSignature input output

-- represents a function that takes no arguments
function' :: forall output. String -> FunctionSignature' output

-- example of defining array_agg for integer inputs
int_array_agg :: FunctionSignature Int (Maybe (Array Int))
int_array_agg = function "array_agg"
```

Be aware that functions must be aliased

```haskell
selectCoalesce :: Select (As "u" (PgFunction (Tuple (Proxy "id") Int) (Tuple (Proxy "id") Int) Users (Maybe Int))) (u :: Maybe Int) _
selectCoalesce = select (coalesce (id /\ 4) # as u) # from users
```

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
from :: forall f q columns sql. ToFrom f q columns => Resume q (From f columns E) sql => f -> q -> sql
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

In the case of overlapping columns in joined sources, an alias is required for disambiguation. For example, if the two following tables are joined

```haskell
type T1 = (
    id :: Int,
    name :: String,
    joined :: Date
)

type T2 = (
    id :: Int,
    name :: String,
    birthday :: Date
)
```

the columns `id` and `name` are not visible -- they must be accessed through `(alias ... colum)` since they are not unique. `birthday` and `joined` however are not repeated in both tables and can be accessed both with and without an alias.

1. INNER JOIN

Returns a cartesian product of both expressions

```haskell
queryInnerJoin :: Select (Path "u" "name") ("u.name" :: String) _
queryInnerJoin = select (u ... name) # from ((messages # as m) `join` (users # as u) # on (m ... sender .=. u ... id)) -- SELECT u.name FROM messages AS m INNER JOIN users AS u ON m.sender = u.id

queryInnerJoin2 :: Select (Tuple (Proxy "name") (Proxy "sender")) ( name :: String, sender :: Int) _
queryInnerJoin2 = select (name /\ sender) # from ((select sender # from messages # as m) `join` (users # as u) # on (m ... sender .=. u ... id)) -- SELECT name, sender FROM (SELECT "sender" FROM messages) AS m INNER JOIN users AS u ON m.sender = u.id
```

2. LEFT OUTER JOIN

Returns a cartesian product of both expressions plus each row in the left hand expression that had no match on the right side. Right side columns will become `Maybe` in the projection type.

```haskell
queryOuterJoin :: Select (Tuple (Proxy "name") (Path "m" "sender")) ("name" :: Maybe String, "m.sender" :: Int) _
queryOuterJoin = select (name /\ m ... sender) # from ((messages # as m) `leftJoin` (users # as u) # on (m ... sender .=. u ... id)) -- SELECT name, m.sender FROM messages AS m OUTER JOIN users AS u ON m.sender = u.id
```

### GROUP BY

```haskell
groupBy:: forall f s q sql grouped columns. ToGroupBy q s columns => GroupByFields f columns grouped => ValidGroupByProjection s grouped => Resume q (GroupBy f E) sql => f -> q -> sql
```

Expectedly, GROUP BY queries limit SELECT projections to grouped columns or aggregations

```haskell
selectGroupBy :: _
selectGroupBy = select ((count id # as b) /\ name) # from users # groupBy (id /\ name) # orderBy id
```

### ORDER BY

```haskell
orderBy :: forall f q sql. ToOrderBy f q => Resume q (OrderBy f E) sql => f -> q -> sql
```

Currently, ORDER BY statements can sort queries only by columns. Note that in DISTINCT queries only the projected columns can be used for sorting.

```haskell
selectOrderBy :: Select (Proxy "name") (name :: String) (From (Table "users" Users ) Users (OrderBy (Proxy "id") E))
selectOrderBy = select name # from users # orderBy id
```

### LIMIT

```haskell
limit :: forall q sql. ToLimit q => Resume q (Limit E) sql => Int -> q -> sql
```

LIMIT must always follow ORDER BY (or OFFSET), as otherwise query order is unspecified.

```haskell
selectLimit :: Select (Proxy "name") (name :: String) (From (Table "users" Users ) Users (OrderBy (Proxy "id") (Limit E)))
selectLimit = select name # from users # orderBy id # limit 1
```

Only number literals are currently supported.

### OFFSET

```haskell
offset :: forall q sql. ToOffset q => Resume q (Offset E) sql => Int -> q -> sql
```

OFFSET must always follow ORDER BY (or LIMIT), as otherwise query order is unspecified.

```haskell
selectOffset :: Select (Proxy "name") (name :: String) (From (Table "users" Users ) Users (OrderBy (Proxy "id") (Offset E)))
selectOffset = select name # from users # orderBy id # offset 5
```

Only number literal are currently supported.

### UNION

```haskell
union :: forall q r. ToUnion q r => q -> r -> Union q r

unionAll :: forall q r. ToUnion q r => q -> r -> Union q r
```

UNION removes duplicates; UNION ALL keeps results as it is. Right and left hand side projections types and column count must match.

```haskell
selectUnion :: _
selectUnion = (select id # from users # wher (name .=. "mary")) `union` (select id # from users # wher (name .=. "john"))
```

## WHERE

```haskell
wher :: forall c q sql. ToWhere c q => Resume q (Where c E) sql => c -> q -> sql
```

The usual operators (e.g., equals, not equals, greater/lesser than, etc.) are surrounded by dots (e.g., `.=.`, `.<>.`, `.>.`, `.<.`). In addition, `NOT`, `EXISTS`, `IN` and `IS NOT NULL` are currently supported. `AND` and `OR` are represented by the operators `.&&.` and `.||.` to help avoiding brackets.

Literal values are replaced with Postgres parameters.

```haskell
selectWhereEquals :: _
selectWhereEquals = select recipient # from messages # wher (sender .=. 1) -- SELECT recipient FROM messages WHERE sender = $1

selectWhereAnd :: _
selectWhereAnd = select id # from users # wher (name .=. "josh" .&&. name .<>. surname) -- SELECT id FROM users WHERE name = $1 AND name <> surname

selectWhereOr :: _
selectWhereOr = select id # from users # wher (name .=. "mary" .||. name .=. surname) -- SELECT id FROM users WHERE name = $1 OR name = surname

selectWhereIn :: _
selectWhereIn = select id # from users # wher (id `in_` [ 3, 4, 5 ]) -- SELECT id FROM users WHERE id IN $1

selectWhereExists :: _
selectWhereExists = select id # from users # wher (exists $ select id # from users)
```

## INSERT

INSERT can be single or multiple values; any number of columns can be specified as long mandatory columns are included.

```haskell
insertSingle :: _
insertSingle = insert # into users (name) # values ("mary")

insertDefault :: _
insertDefault = insert # into messages (sender /\ recipient /\ date) # values (5 /\ 9 /\ Default)

insertMultiple :: _
insertMultiple = insert # into users (name /\ birthday) # values ["mary" /\ Just date, "josh" /\ Nothing]
```

## UPDATE

UPDATE queries can be as it is or with a WHERE clause.

```haskell
updateQuery :: _
updateQuery = update users # set ((name .=. "Mary") /\ (birthday .=. Nothing))

updateWhere :: _
updateWhere = update users # set ((name .=. "Mary") /\ (birthday .=. Nothing)) # where (id .=. 4)
```

## DELETE

DELETE queries can be as it is or with a WHERE clause.

```haskell
deleteQuery :: Delete (From (Table "users" Users) Users E)
deleteQuery = delete # from users

deleteWhere :: _
deleteWhere = delete # from users # where (id .=. 4)
```

## RETURNING

RETURNING can output columns from INSERT, UPDATE and DELETE

```haskell
insertReturning :: _
insertReturning = insert # into users (name) # values ("mary") # returning id

updateReturning :: _
updateReturning = update users # set ((name .=. "Mary") /\ (birthday .=. Nothing)) # where (id .=. 4) # returning (id /\ name)

deleteReturning :: _
deleteReturning = delete # from users # returning birthday
```

## AS

AS can be used to alias columns, tables and queries.

```haskell
asColumn :: _
asColumn = select (id # as m) # from users

asTable :: _
asTable = select id # from (users # as m)

asQuery :: _
asQuery = select id # from (select star # from users # as m)
```

Be aware of bracketing, `select id # from (select star # from users # as m)` is parsed as `SELECT id FROM (SELECT * FROM users) AS m` whereas `select id # from (select star # from users) # as m` results in a type error.

## PREPARE

Prepared statements can be done with `prepare`.

```haskell
prepare :: forall q. ToPrepare q => Plan -> q -> Prepare q
```

## CREATE

## ALTER

## DROP

Only the plan name is required. Parameters will be automatically parsed from the query.

<a href="/index" class="direction previous">Previous: Getting started</a>
<a href="/mapper" class="direction">Next: Query Mapper</a>
