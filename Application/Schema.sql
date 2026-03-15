-- ALTER DATABASE app SET timezone = 'UTC';
CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language plpgsql;
CREATE TYPE market_status AS ENUM ('market_status_draft', 'market_status_open', 'market_status_closed', 'market_status_resolved', 'market_status_refunded');
CREATE TABLE users (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    nickname TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX users_nickname_index ON users (LOWER(nickname));
CREATE TRIGGER update_users_updated_at BEFORE UPDATE ON users FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE passkeys (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    credential_id BYTEA NOT NULL UNIQUE,
    public_key BYTEA NOT NULL,
    sign_count BIGINT NOT NULL
);
CREATE INDEX passkeys_user_id_index ON passkeys (user_id);
CREATE TABLE admins (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
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
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE,
    sort_idx INT
);
CREATE TABLE markets (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    user_id UUID,
    title TEXT NOT NULL,
    slug TEXT NOT NULL,
    description TEXT NOT NULL,
    category_id UUID NOT NULL,
    beta BIGINT DEFAULT 300 NOT NULL,
    status market_status DEFAULT 'market_status_draft' NOT NULL,
    opened_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    closed_at TIMESTAMP WITH TIME ZONE NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    refunded_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    trades BIGINT DEFAULT 0 NOT NULL,
    volume BIGINT DEFAULT 0 NOT NULL,
    turnover BIGINT DEFAULT 0 NOT NULL,
    outcome_asset_id UUID DEFAULT NULL
);
CREATE INDEX markets_user_id_index ON markets (user_id);
CREATE INDEX markets_status_index ON markets (status);
CREATE INDEX markets_created_at_index ON markets (created_at);
CREATE INDEX markets_opened_at_index ON markets (opened_at);
CREATE INDEX markets_closed_at_index ON markets (closed_at);
CREATE UNIQUE INDEX markets_category_id_slug_index ON markets (category_id, slug);
CREATE TRIGGER update_markets_updated_at BEFORE UPDATE ON markets FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE assets (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    market_id UUID NOT NULL,
    name TEXT NOT NULL,
    symbol TEXT NOT NULL,
    quantity BIGINT DEFAULT 0 NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX assets_market_id_symbol_index ON assets (market_id, symbol);
CREATE TRIGGER update_assets_updated_at BEFORE UPDATE ON assets FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE wallets (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL UNIQUE,
    amount BIGINT DEFAULT 100000 NOT NULL
);
CREATE INDEX wallets_user_id_index ON wallets (user_id);
CREATE TABLE transactions (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    market_id UUID NOT NULL,
    asset_id UUID NOT NULL,
    quantity BIGINT DEFAULT 0 NOT NULL,
    cash_flow BIGINT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    price_before DOUBLE PRECISION DEFAULT 0 NOT NULL,
    price_after DOUBLE PRECISION DEFAULT 0 NOT NULL,
    market_state JSONB DEFAULT '{}'::JSONB NOT NULL
);
CREATE INDEX transactions_user_id_index ON transactions (user_id);
CREATE INDEX transactions_market_id_asset_id_index ON transactions (market_id, asset_id);
CREATE INDEX transactions_created_at_index ON transactions (created_at);
CREATE TABLE positions (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    market_id UUID NOT NULL,
    asset_id UUID NOT NULL,
    quantity BIGINT DEFAULT 0 NOT NULL,
    invested BIGINT DEFAULT 0 NOT NULL,
    received BIGINT DEFAULT 0 NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX positions_user_id_index ON positions (user_id);
CREATE INDEX positions_market_id_index ON positions (market_id);
CREATE INDEX positions_asset_id_index ON positions (asset_id);
CREATE UNIQUE INDEX positions_user_id_asset_id_index ON positions (user_id, asset_id);
CREATE TRIGGER update_positions_updated_at BEFORE UPDATE ON positions FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
-- CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_timed_out', 'job_status_succeeded', 'job_status_retry');
CREATE TABLE close_market_jobs (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    market_id UUID NOT NULL UNIQUE
);
CREATE INDEX close_market_jobs_market_id_index ON close_market_jobs (market_id);
CREATE TABLE market_chat_messages (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    market_id UUID NOT NULL,
    user_id UUID NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX market_chat_messages_market_id_created_at_index ON market_chat_messages (market_id, created_at);
ALTER TABLE assets ADD CONSTRAINT assets_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
ALTER TABLE close_market_jobs ADD CONSTRAINT close_market_jobs_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
ALTER TABLE market_chat_messages ADD CONSTRAINT market_chat_messages_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
ALTER TABLE market_chat_messages ADD CONSTRAINT market_chat_messages_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE markets ADD CONSTRAINT markets_ref_category_id FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE NO ACTION;
ALTER TABLE markets ADD CONSTRAINT markets_ref_outcome_asset_id FOREIGN KEY (outcome_asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
ALTER TABLE markets ADD CONSTRAINT markets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE SET NULL;
ALTER TABLE passkeys ADD CONSTRAINT passkeys_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE positions ADD CONSTRAINT positions_ref_asset_id FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
ALTER TABLE positions ADD CONSTRAINT positions_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE NO ACTION;
ALTER TABLE positions ADD CONSTRAINT positions_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_asset_id FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE NO ACTION;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE wallets ADD CONSTRAINT wallets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
