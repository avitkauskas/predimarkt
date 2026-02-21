ALTER TABLE close_market_jobs DROP CONSTRAINT close_market_jobs_ref_market_id;
ALTER TABLE close_market_jobs ADD CONSTRAINT close_market_jobs_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
