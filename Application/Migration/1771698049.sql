DROP INDEX transactions_asset_id_index;
DROP INDEX transactions_market_id_index;
CREATE INDEX transactions_market_id_asset_id_index ON transactions (market_id, asset_id);
