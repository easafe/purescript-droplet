---
layout: default
title: Getting started
---

## Composable, type-safe eDSL and query mapper for PureScript targeting PostgreSQL

Droplet is a bit different from other PureScript (or Haskell) SQL libraries. There is no monads, functions to yield columns, or a higher level API abstracting away from the generated SQL query. Instead, the eDSL is made out exclusively of combinators (nearly) identical to their SQL statement counterparts. Likewise, the output of a query is automatically inferred from its projection with almost no need for type annotations or boilerplate mapping type class instances.

This guide first covers the syntax used by the eDSL, how to run queries using a query mapper and finally migrations. The eDSL aims to make the keywords you already know (SELECT, WHERE, ORDER BY, JOIN, etc) composable. Likewise, the query mapper is independent from the eDSL -- it can be also used for unsafe queries. Migrations are accomplished by API or using the CLI.

Installation:

```
npm i big-integer pg

spago install droplet
```

If you'd like to try the code examples, the following schema will be used throughout

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


<a href="/edsl" class="direction">Next: eDSL</a>
