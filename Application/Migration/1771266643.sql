ALTER TABLE markets RENAME COLUMN resolved_asset_id TO outcome_asset_id;
ALTER TABLE markets DROP CONSTRAINT markets_ref_resolved_asset_id;
ALTER TABLE markets ADD CONSTRAINT markets_ref_outcome_asset_id FOREIGN KEY (outcome_asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
