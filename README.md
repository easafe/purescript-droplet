## purescript-droplet

Composable, type-safe EDSL and query mapper for PureScript targeting PostgreSQL

### Getting started

Droplet's is a bit different from other PureScript (or Haskell) SQL libraries. There is no monads, functions to yield columns, or a higher level API abstracting away from the generated SQL query. Instead, the EDSL is made out exclusively of combinators (nearly) identical to their SQL statement counterparts. Likewise, the output of a query is automatically inferred from its projection with almost no need for type annotations or boilerplate type class instances.

As an example, let's create a user table for some imaginary application

```sql
create table users (
    id integer generated always as identity primary key,
    name text not null,
    joined date default (now() at time zone 'utc'),
);
```

So we don't have to repeat it in every query, let's define some types for this table as well

```purescript
-- representation of the table itself
users :: Table "users" Users
users = Table

-- representation of the table's columns definitions
type Users = (
    id :: Auto Int, -- identity column
    name :: String,
    joined :: Default Date, -- column with default
)

-- representation of column names to be used in queries
id :: Proxy "id"
id = Proxy

name :: Proxy "name"
name = Proxy

joined :: Proxy "joined"
joined = Proxy

-- alias
u :: Proxy "u"
u = Proxy
```

#### Writing queries

If we wanted to insert and then fetch records from the `users` tables, we could write the following SQL queries

```sql
INSERT INTO users (name, joined) values ('Mary Sue', '2000-01-01')

SELECT * FROM users
```

Using the types above, the same in Droplet reads

```purescript
insert # into users (name /\ joined) # values ("Mary Sue" /\ canonicalDate year month day)

select star # from users
```

Likewise, filtering/modifying records

```sql
UPDATE users SET name = 'Gary Stu' WHERE name = 'Mary Sue'

SELECT name, joined FROM users WHERE id <> 23 AS u
```

```purescript
update users # set (name /\ 'Gary Stu') # wher (name .=. 'Mary Sue')

select (name /\ joined) # from users # wher (id .<>. 23) # as u
```

... and so on and so forth. That is one of the goals of the EDSL: translating queries from PureScript to SQL is direct, with no need for guesswork.

However, not all queries can be expressed with the EDSL, specially ones that would result in syntax or type errors during runtime. In fact, that's another goal of the EDSL: if a query type checks, it should sucessfully run without any errors. For that reason, besides catching many kinds of mistakes, the DSL does not allow ambiguous or error prone operations. For example:

* Invalid or unmatching columns

From user input or field names

```purescript
select (Proxy :: Proxy "brithday") # from users

select name # from users # wher (id .=. name)

select name # from users # wher (id .=. "23")
```

In insert/update lists

```purescript
insert # into users (id /\ name) # values (23 /\ "Mary Sue") -- cannot insert identity column

insert # into users joined # values (canonicalDate year month day) -- name is a required field
```

* Invalid column subqueries

Subqueries can only be used as columns if they return a single result column

```purescript
select (select (id /\ name) # from users # where id .=. 23) # from users -- type error
```

* Selecting from subqueries must always include an alias

The equivalent of

```sql
SELECT * from (SELECT id, name FROM users)
```

doesn't type check. `as` is required to make this query valid

```purescript
select star # from (select (id /\ name) # from users # as u)
```

* More than one column with the same name

```purescript
select (id /\ (select id # from users # where id .=. 23)) # from users
```

doesn't type check. The solution again is to alias the column, e.g.,

```purescript
select (id /\ (select id # from users # where id .=. 23 # as u)) # from users
```

The last goal of the EDSL is to never require type annotations to type check valid queries. Query types can become quite verbose, but as a rule of thumb, combinators' input are restricted by type classes, they compose left to right and express next statements in their last type variable. For example, the type of `select` is

```purescript
select :: forall r s projection. ToSelect r s projection => r -> Select s projection E
```

`projection` tells the final output, devoid of table definitions like `Auto` or `Default`. So one of the above queries' type becomes

```purescript
Select (Tuple (Proxy "id") (Proxy "name"))
  ( id :: Int
  , name :: String
  )
  (From
     (Table "users"
        ( id :: Auto Int
        , joined :: Default Date
        , name :: String
        )
     )
     ( id :: Auto Int
     , joined :: Default Date
     , name :: String
     )
     (Where (Op (Proxy "id") Int) E)
  )
select (id /\ name) # from users # wher (id .=. 3)
```

#### Running queries

Database access is done through a query mapper. All this means is that the projection type of a query is matched with the actual data returned by the database. Rows are turned into records with columns as fields. For example, in the case of a query like

```purescript
select (id /\ (joined # as u)) # from users
```

Droplet infers that the output type is `( id :: Int, u :: Date )`. Queries with no projection, e.g. `update`, simply return the empty row type `()`.

The query mapper also does its best to reject invalid or ambiguous queries. Case in point:

* Syntatically invalid naked selects

It is reasonable to write

```purescript
selectIdName = select (id /\ name)
```

to compose later in queries that reference the same columns, but it is not valid SQL on its own.

* User inputed values in queries

Whenever a value is used within a query, it is automatically parsed into a parameter

```purescript
select name # from users # where (id .=. 23)
```

generates

```sql
SELECT name FROM users WHERE id = $1
```

using PostgreSQL server parameters. This is done to avoid SQL injection. For prepared statements, see `Dropelet.prepare`.

* Unnamed columns

Literal values, like numbers or dates, must be aliased. Same for functions like `COUNT` or `MAX`

```purescript
select 3 # from users -- type error

select (3 # as u) # from users -- valid
```


### Licensing

Wrapper code around [pg](https://github.com/brianc/node-postgres) was adapted from [purescript-postgresql-client](https://github.com/rightfold/purescript-postgresql-client) so its license has been included in [PURESCRIPT-POSTGRESQL-CLIENT-LICENSE](PURESCRIPT-POSTGRESQL-CLIENT-LICENSE)