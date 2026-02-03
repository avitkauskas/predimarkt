ALTER TABLE assets DROP COLUMN status;
ALTER TABLE assets DROP COLUMN resolved_at;
ALTER TABLE assets DROP COLUMN refunded_at;
ALTER TABLE assets DROP COLUMN created_at;
DROP TYPE asset_status;