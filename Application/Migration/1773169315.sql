CREATE TABLE passkeys (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    credential_id BYTEA NOT NULL,
    public_key BYTEA NOT NULL,
    sign_count BIGINT NOT NULL
);
ALTER TABLE passkeys ADD CONSTRAINT passkeys_credential_id_key UNIQUE(credential_id);
CREATE INDEX passkeys_user_id_index ON passkeys (user_id);
ALTER TABLE passkeys ADD CONSTRAINT passkeys_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
