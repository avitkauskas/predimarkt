CREATE TABLE wallets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    balance_cents BIGINT DEFAULT 0 NOT NULL
);
ALTER TABLE wallets ADD CONSTRAINT wallets_user_id_key UNIQUE(user_id);
CREATE INDEX wallets_user_id_index ON wallets (user_id);
ALTER TABLE wallets ADD CONSTRAINT wallets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
