CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language plpgsql;
-- ALTER DATABASE app SET timezone = 'UTC';
CREATE TYPE market_statuses AS ENUM ('draft', 'open', 'closed', 'resolved', 'refunded');
CREATE TYPE asset_statuses AS ENUM ('open', 'resolved', 'refunded');
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    nickname TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX users_email_index ON users (LOWER(email));
CREATE UNIQUE INDEX users_nickname_index ON users (LOWER(nickname));
CREATE TRIGGER update_users_updated_at BEFORE UPDATE ON users FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE categories (
    id SERIAL PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE
);
CREATE TABLE markets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    title TEXT NOT NULL,
    slug TEXT NOT NULL,
    description TEXT NOT NULL,
    category_id INTEGER NOT NULL,
    beta DOUBLE PRECISION DEFAULT 140.0 NOT NULL,
    status market_statuses DEFAULT 'draft' NOT NULL,
    opened_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    closed_at TIMESTAMP WITH TIME ZONE NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    refunded_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX markets_user_id_index ON markets (user_id);
CREATE INDEX markets_status_index ON markets (status);
CREATE INDEX markets_created_at_index ON markets (created_at);
CREATE INDEX markets_opened_at_index ON markets (opened_at);
CREATE INDEX markets_closed_at_index ON markets (closed_at);
CREATE UNIQUE INDEX markets_category_id_slug_index ON markets (category_id, slug);
CREATE TRIGGER update_markets_updated_at BEFORE UPDATE ON markets FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE assets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    market_id UUID NOT NULL,
    name TEXT NOT NULL,
    label TEXT NOT NULL,
    status asset_statuses DEFAULT 'open' NOT NULL,
    quantity DOUBLE PRECISION DEFAULT 0.0 NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    refunded_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX assets_market_id_index ON assets (market_id);
CREATE TRIGGER update_assets_updated_at BEFORE UPDATE ON assets FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
ALTER TABLE markets ADD CONSTRAINT markets_ref_category_id FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE NO ACTION;
ALTER TABLE markets ADD CONSTRAINT markets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE SET NULL;
ALTER TABLE assets ADD CONSTRAINT assets_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
