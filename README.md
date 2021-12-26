## purescript-droplet ![build status](https://github.com/easafe/purescript-droplet/actions/workflows/CI.yml/badge.svg)

Composable, type-safe eDSL and query mapper for PureScript targeting PostgreSQL

* eDSL made out of combinators (almost) identical to their SQL counterparts

* Generated SQL matches eDSL one-to-one, and is guaranteed to be correct and unambiguous

* Supports nearly all common SQL operations for ```SELECT```, ```INSERT```, ```UPDATE```, ``DELETE`` and others, including joins, aggregations, subqueries, etc

* Very little boilerplate: query results are automatically mapped to records, (valid) queries never require type annotations

* Migration support via [pebble](https://github.com/easafe/haskell-pebble)

### Documentation

See the [project page](https://droplet.asafe.dev/) for an in-depth look, or [pursuit](https://pursuit.purescript.org/packages/purescript-droplet) for API docs

### Quick start

Install

```
npm i big-integer pg @easafe/pebble

spago install droplet
```

Create some tables

```sql
create table users (
    id integer generated always as identity primary key,
    name text not null,
    birthday date null
);

create table messages (
    id integer generated always as identity primary key,
    sender integer not null,
    recipient integer not null,
    date timestamptz default now(),

    constraint sender_user foreign key (sender) references users(id),
    constraint recipient_user foreign key (recipient) references users(id)
);
```

Define some types for your SQL

```purescript
-- representation of the table itself
type UsersTable = Table "users" Users

users :: UsersTable
users = Table

messages :: Table "messages" Messages
messages = Table

-- representation of the table's columns definitions
type Users = (
    id :: Column Int (PrimaryKey /\ Identity), -- primary key, generated always as identity
    name :: String,
    birthday :: Maybe Date -- nullable column
)

type Messages = (
    id :: Column Int (PrimaryKey /\ Identity),
    sender :: Column Int (ForeignKey "id" UsersTable), -- foreign key to table users
    recipient :: Column Int (ForeignKey "id" UsersTable),
    date :: Column DateTime (Constraint "date_default_messages" Default) -- column with named default constrain
)

-- representation of column names to be used in queries
id :: Proxy "id"
id = Proxy

name :: Proxy "name"
name = Proxy

birthday :: Proxy "birthday"
birthday = Proxy

sender :: Proxy "sender"
sender = Proxy

recipient :: Proxy "recipient"
recipient = Proxy

date :: Proxy "date"
date = Proxy

-- alias
u :: Proxy "u"
u = Proxy

m :: Proxy "m"
m = Proxy

t :: Proxy "t"
t = Proxy
```

(Don't worry, table creation, typing and migration can be automated with [pebble](https://github.com/easafe/haskell-pebble))

Prepare some queries

```purescript
mary :: _
mary =
    insert #
    into users (name) #
    values ("Mary Sue") # -- `name` is the only required field; it would be a type error to set `id`, as it is an identity column
    returning id -- output inserted `id`

gary :: Date -> _
gary bday =
    insert #
    into users (name /\ birthday) # -- tuple for field list
    values ("Gary Stu" /\ Just bday) # -- set the nullable field `birthday`
    returning id

chat :: Int -> Int -> _
chat from to = insert # into messages (sender /\ recipient) # values (from /\ to) -- `date` has a default value

selectMessages :: _
selectMessages =
      select (id /\ date) #
      from messages

selectUserMessages :: Int -> _
selectUserMessages userId =
      selectMessages #
      wher (id .=. userId) -- SQL operators are surrounded by dots; we can compare `id` to `userId` as type wrappers such as `Auto` are automatically stripped

joinUserMessages :: _
joinUserMessages =
      select (u ... name /\ -- `...` is equivalent to table.column
              (t ... name # as recipient) /\ -- `name` is displayed as recipient
              date) #
      from (((messages # as m)
            `join`
            (users # as u) #
            on (m ... sender .=. u ... id))
            `join`
            (users # as t) #
            on (m ... recipient .=. t ... id))
```

Connect to the database

```purescript
connectionInfo :: Configuration
connectionInfo = (Driver.defaultConfiguration "database") {
      user = Just "user"
}

example :: Aff Unit
example = do
      pool <- liftEffect $ Pool.newPool connectionInfo -- connection pool from PostgreSQL
      Driver.withConnection pool case _ of
            Left error -> pure unit -- or some more sensible handling
            Right connection -> runSql connection
```

Run queries

```purescript
runSql :: Connection -> Aff Unit
runSql connection = do
      now <- liftEffect Now.nowDate
      mRow <- Driver.single connection mary -- run a query that returns a single row
      gRow <- Driver.single connection $ gary now
      case mRow, gRow of
            Right (Just {id: mId}), Right (Just {id: gId}) -> void do
                  mErr <- Driver.execute connection $ chat mId gId -- run a query that doesn't produce an output
                  gErr <- Driver.execute connection $ chat gId mId

                  mMessages <- Driver.query connection $ selectUserMessages mId -- run a query that returns rows
                  gMessages <- Driver.query connection $ selectUserMessages gId -- rows are always records, the keys are the projected columns
                  Driver.query connection joinUserMessages

            _, _ -> pure unit
```

### Licensing

Wrapper code around [pg](https://github.com/brianc/node-postgres) was adapted from [purescript-postgresql-client](https://github.com/rightfold/purescript-postgresql-client) so its license has been included in [PURESCRIPT-POSTGRESQL-CLIENT-LICENSE](PURESCRIPT-POSTGRESQL-CLIENT-LICENSE)

### Funding

If this project is useful for you, consider [throwing a buck](https://asafe.dev/donate) to keep development possible
