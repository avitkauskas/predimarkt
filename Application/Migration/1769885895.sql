ALTER TABLE assets DROP COLUMN quantity;
ALTER TABLE assets ADD COLUMN quantity INT DEFAULT 0 NOT NULL;
ALTER TABLE wallets RENAME COLUMN balance_cents TO amount_cents;
CREATE TABLE transactions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    market_id UUID NOT NULL,
    asset_id UUID NOT NULL,
    quantity INT NOT NULL,
    amount_cents BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
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
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE INDEX holdings_user_id_index ON holdings (user_id);
CREATE INDEX holdings_market_id_index ON holdings (market_id);
CREATE INDEX holdings_asset_id_index ON holdings (asset_id);
CREATE TRIGGER update_holdings_updated_at BEFORE UPDATE ON holdings FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
ALTER TABLE holdings ADD CONSTRAINT holdings_ref_asset_id FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
ALTER TABLE holdings ADD CONSTRAINT holdings_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE NO ACTION;
ALTER TABLE holdings ADD CONSTRAINT holdings_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_asset_id FOREIGN KEY (asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE NO ACTION;
ALTER TABLE transactions ADD CONSTRAINT transactions_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
