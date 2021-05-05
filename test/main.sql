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
      date timestamp default (now() at time zone 'utc'),
      sent bool not null,

      constraint sender_user foreign key (sender) references users(id),
      constraint recipient_user foreign key (recipient) references users(id)
);

create or replace function truncate_tables()
  returns void as
$body$
begin
      truncate table users restart identity cascade;
      truncate table messages restart identity cascade ;
end;
  $body$
  language plpgsql;


--while we dont support select
insert into users (name, surname, birthday) values ('josh', 'j.', '1990-01-01');
insert into users (name, surname, birthday) values ('mary', 'sue', '1900-11-11');

insert into messages (sender, recipient, sent) values (1, 2, true);
insert into messages (sender, recipient, sent) values (2, 1, true);

