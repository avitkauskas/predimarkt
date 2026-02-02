-- ALTER DATABASE app SET timezone = 'UTC';
CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language plpgsql;
CREATE TYPE market_status AS ENUM ('market_status_draft', 'market_status_open', 'market_status_closed', 'market_status_resolved', 'market_status_refunded');
CREATE TYPE asset_status AS ENUM ('asset_status_open', 'asset_status_resolved', 'asset_status_refunded');
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
CREATE TABLE admins (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX admins_email_index ON admins (LOWER(email));
CREATE TRIGGER update_admins_updated_at BEFORE UPDATE ON admins FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE categories (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE,
    sort_idx INT
);
CREATE TABLE markets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID,
    title TEXT NOT NULL,
    slug TEXT NOT NULL,
    description TEXT NOT NULL,
    category_id UUID NOT NULL,
    beta INT DEFAULT 300 NOT NULL,
    status market_status DEFAULT 'market_status_draft' NOT NULL,
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
    symbol TEXT NOT NULL,
    status asset_status DEFAULT 'asset_status_open' NOT NULL,
    quantity INT DEFAULT 0 NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    refunded_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX assets_market_id_index ON assets (market_id);
CREATE TRIGGER update_assets_updated_at BEFORE UPDATE ON assets FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE wallets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL UNIQUE,
    amount_cents BIGINT DEFAULT 100000 NOT NULL
);
CREATE INDEX wallets_user_id_index ON wallets (user_id);
CREATE TABLE transactions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    market_id UUID NOT NULL,
    asset_id UUID NOT NULL,
    quantity INT NOT NULL,
    amount_cents BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX transactions_user_id_index ON transactions (user_id);
CREATE INDEX transactions_market_id_index ON transactions (market_id);
CREATE INDEX transactions_asset_id_index ON transactions (asset_id);
CREATE INDEX transactions_created_at_index ON transactions (created_at);
CREATE TABLE holdings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    market_id UUID NOT NULL,
    asset_id UUID NOT NULL,
    quantity INT NOT NULL,
    amount_cents BIGINT NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX holdings_user_id_index ON holdings (user_id);
CREATE INDEX holdings_market_id_index ON holdings (market_id);
CREATE INDEX holdings_asset_id_index ON holdings (asset_id);
CREATE UNIQUE INDEX holdings_user_id_asset_id_index ON holdings (user_id, asset_id);
CREATE TRIGGER update_holdings_updated_at BEFORE UPDATE ON holdings FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
ALTER TABLE assets ADD CONSTRAINT assets_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
ALTER TABLE holdings ADD CONSTRAINT holdings_ref_asset_id FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
ALTER TABLE holdings ADD CONSTRAINT holdings_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE NO ACTION;
ALTER TABLE holdings ADD CONSTRAINT holdings_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE markets ADD CONSTRAINT markets_ref_category_id FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE NO ACTION;
ALTER TABLE markets ADD CONSTRAINT markets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE SET NULL;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_asset_id FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE NO ACTION;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE wallets ADD CONSTRAINT wallets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
