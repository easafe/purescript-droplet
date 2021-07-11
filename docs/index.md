---
layout: default
title: Getting started
---

## Composable, type-safe EDSL and query mapper for PureScript targeting PostgreSQL

Droplet is a bit different from other PureScript (or Haskell) SQL libraries. There is no monads, functions to yield columns, or a higher level API abstracting away from the generated SQL query. Instead, the EDSL is made out exclusively of combinators (nearly) identical to their SQL statement counterparts. Likewise, the output of a query is automatically inferred from its projection with almost no need for type annotations or boilerplate type class instances.

This guide first cover the syntax used by the EDSL, then how to run queries using a query mapper. Since the EDSL is just plain old PureScript data types, it can be be stored, composed, passed around like any other value. Likewise, the query mapper is independent from the EDSL -- it can be also used for unsafe queries. The following schema will be used throughout

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

with the following type definitions

```haskell
type Users = (
    id :: Auto Int,
    name :: String,
    birthday :: Maybe Date
)

type Messages = (
    id :: Auto Int,
    sender :: Int,
    recipient :: Int,
    date :: Default DateTime
)

users :: Table "users" Users
users = Table

messages :: Table "messages" Messages
messages = Table

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

m :: Proxy "m"
m = Proxy

u :: Proxy "u"
u = Proxy

t :: Proxy "t"
t = Proxy
```

<a href="/edsl" class="direction">Next: EDSL</a>
