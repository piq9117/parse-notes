CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE note_titles (
  id INTEGER NOT NULL PRIMARY KEY,
  title TEXT NOT NULL,
  note_id TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
CREATE TABLE note_bodies (
  id INTEGER NOT NULL PRIMARY KEY,
  note_title__id INTEGER NOT NULL REFERENCES note_titles(id),
  body TEXT NOT NULL,
  note_id TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20241018031041'),
  ('20241018031311');
