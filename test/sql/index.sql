create or replace function utc_now()
    returns timestamptz as
$body$
begin
    return now() at time zone 'utc';
end;
    $body$
    language plpgsql;

create or replace function recipient_default()
    returns integer as
$body$
begin
    return (select coalesce(max(recipient), 1) from default_columns); -- don't try it at home
end;
    $body$
    language plpgsql;

create table users (
    id integer generated always as identity primary key,
    name text not null,
    surname text not null,
    birthday date default (utc_now()),
    joined date default (utc_now())
);

create table messages (
    id integer generated always as identity primary key,
    sender integer not null,
    recipient integer not null,
    date timestamp without time zone default (utc_now()),
    second_date timestamp with time zone default (utc_now()),
    sent bool not null,

    constraint sender_user foreign key (sender) references users(id),
    constraint recipient_user foreign key (recipient) references users(id)
);

create table tags (
    id integer generated always as identity primary key,
    name text not null,
    created date,
    by integer,

    constraint by_user foreign key (by) references users(id)
);

create table maybe_keys (
    id integer primary key
);

create table unique_values (
    name text not null unique,
    by integer unique
);

create table default_columns (
    recipient integer unique not null default (recipient_default()),
    sender integer default (44)
);

create table double_primary_key (
    id integer generated always as identity,
    second_id integer generated always as identity ,

    constraint pk_double_primary_key primary key (id, second_id)
);

create table composite (
    id integer generated always as identity,
    second_id integer not null,
    name text not null,
    created date,
    sender integer,
    recipient integer,

    constraint pk_composite primary key (id, second_id),
    constraint sr_user foreign key (sender, recipient) references double_primary_key(id, second_id )
);

create or replace function truncate_tables()
    returns void as
$body$
begin
    truncate table users restart identity cascade;
    truncate table messages restart identity cascade;
    truncate table tags restart identity cascade;
    truncate table maybe_keys cascade;
    truncate table unique_values cascade;
    truncate table default_columns cascade;
    truncate table double_primary_key cascade;
    truncate table composite cascade;
end;
  $body$
  language plpgsql;

create or replace function date_part_age(part text, tm timestamp with time zone)
    returns integer as
$body$
begin
     return date_part(part, age(now (), tm));
end;
    $body$
    language plpgsql;

create or replace function fire_missiles(a integer, b integer)
    returns void as
$body$
begin

end;
    $body$
    language plpgsql;