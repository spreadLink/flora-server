create table users (
  user_id uuid primary key,
  username text unique,
  display_name text,
  email text unique,
  password text,
  imported bool,
  created_at timestamptz,
  updated_at timestamptz
);

create unique index on users (lower(username));
