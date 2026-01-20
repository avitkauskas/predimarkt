CREATE TABLE categories (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    slug TEXT NOT NULL
);
ALTER TABLE categories ADD CONSTRAINT categories_slug_key UNIQUE(slug);
CREATE TABLE markets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    title TEXT NOT NULL,
    slug TEXT NOT NULL,
    description TEXT NOT NULL,
    category_id UUID NOT NULL,
    beta DOUBLE PRECISION DEFAULT 140.0 NOT NULL,
    status market_statuses DEFAULT 'draft' NOT NULL,
    opened_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    closed_at TIMESTAMP WITH TIME ZONE NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    refunded_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE INDEX markets_user_id_index ON markets (user_id);
CREATE INDEX markets_status_index ON markets (status);
CREATE INDEX markets_created_at_index ON markets (created_at);
CREATE INDEX markets_opened_at_index ON markets (opened_at);
CREATE INDEX markets_closed_at_index ON markets (closed_at);
CREATE UNIQUE INDEX markets_category_id_slug_index ON markets (category_id, slug);
CREATE TRIGGER update_markets_updated_at BEFORE UPDATE ON markets FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
ALTER TABLE markets ADD CONSTRAINT markets_ref_category_id FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE NO ACTION;
ALTER TABLE markets ADD CONSTRAINT markets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE SET NULL;
CREATE TABLE assets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    market_id UUID NOT NULL,
    name TEXT NOT NULL,
    label TEXT NOT NULL,
    status asset_statuses DEFAULT 'open' NOT NULL,
    quantity DOUBLE PRECISION DEFAULT 0.0 NOT NULL,
    resolved_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    refunded_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE INDEX assets_market_id_index ON assets (market_id);
CREATE TRIGGER update_assets_updated_at BEFORE UPDATE ON assets FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
ALTER TABLE assets ADD CONSTRAINT assets_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
