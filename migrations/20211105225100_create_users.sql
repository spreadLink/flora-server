create extension if not exists citext;

create table users (
  user_id UUID PRIMARY KEY,
  username citext unique,
  display_name TEXT,
  email TEXT unique,
  password TEXT,
  created_at timestamptz,
  updated_at timestamptz
);

create index on users (username);
