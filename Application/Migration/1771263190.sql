ALTER TABLE markets ADD COLUMN resolved_asset_id UUID DEFAULT null;
ALTER TABLE markets ADD CONSTRAINT markets_ref_resolved_asset_id FOREIGN KEY (resolved_asset_id) REFERENCES assets (id) ON DELETE NO ACTION;
