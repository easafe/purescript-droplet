create table users (
      id integer generated always as identity primary key,
      name text not null,
      surname text not null,
      birthday date default (now() at time zone 'utc'),
      joined date default (now() at time zone 'utc')
);

create table messages (
      id integer generated always as identity primary key,
      sender integer not null,
      recipient integer not null,
      date timestamp without time zone default (now() at time zone 'utc'),
      second_date timestamp with time zone default (now() at time zone 'utc'),
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

create or replace function truncate_tables()
  returns void as
$body$
begin
      truncate table users restart identity cascade;
      truncate table messages restart identity cascade;
      truncate table tags restart identity cascade;
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