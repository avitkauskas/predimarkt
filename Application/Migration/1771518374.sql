ALTER TABLE users ADD COLUMN workos_user_id TEXT NOT NULL;
ALTER TABLE users ADD CONSTRAINT users_workos_user_id_key UNIQUE(workos_user_id);
