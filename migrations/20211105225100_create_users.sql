create table users (
  user_id uuid primary key,
  username text unique not null,
  display_name text not null,
  email text unique not null,
  password text not null,
  imported bool not null,
  created_at timestamptz not null,
  updated_at timestamptz not null
);

create unique index on users (lower(username));
create unique index on users (lower(email));
create unique index on users (imported);
