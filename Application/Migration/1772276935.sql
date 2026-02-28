DROP INDEX assets_market_id_index;
CREATE UNIQUE INDEX assets_market_id_symbol_index ON assets (market_id, symbol);
