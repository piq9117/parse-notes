-- migrate:up
CREATE TABLE IF NOT EXISTS note_bodies (
  id INTEGER NOT NULL PRIMARY KEY,
  note_title__id INTEGER NOT NULL REFERENCES note_titles(id),
  body TEXT NOT NULL,
  note_id TEXT UNIQUE NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- migrate:down

