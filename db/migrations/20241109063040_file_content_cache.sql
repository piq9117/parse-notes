-- migrate:up
CREATE TABLE IF NOT EXISTS file_contents_cache (
  id INTEGER NOT NULL PRIMARY KEY,
  content TEXT NOT NULL
)

-- migrate:down

