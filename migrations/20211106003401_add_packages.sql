create table packages (
  package_id uuid primary key,
  namespace citext not null,
  name citext not null,
  synopsis text not null,
  owner_id uuid references users,
  metadata jsonb not null, -- { homepage, documentation url, repository url, issues url }
  created_at timestamptz not null,
  updated_at timestamptz not null
);

create unique index on packages(name, namespace);
