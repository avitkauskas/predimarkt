ALTER TABLE close_market_jobs ADD COLUMN market_id UUID NOT NULL;
CREATE INDEX close_market_jobs_market_id_index ON close_market_jobs (market_id);
ALTER TABLE close_market_jobs ADD CONSTRAINT close_market_jobs_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE NO ACTION;
