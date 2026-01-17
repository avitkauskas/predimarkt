CREATE FUNCTION set_updated_at() RETURNS TRIGGER AS $$BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;$$ language PLPGSQL;
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    nickname TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp NOT NULL
);
CREATE UNIQUE INDEX users_email_index ON users (lower(email));
CREATE UNIQUE INDEX users_nickname_index ON users (lower(nickname));
CREATE TRIGGER set_updated_at BEFORE UPDATE ON users FOR EACH ROW EXECUTE FUNCTION set_updated_at();
