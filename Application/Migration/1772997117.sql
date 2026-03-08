CREATE TABLE market_chat_messages (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    market_id UUID NOT NULL,
    user_id UUID NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
ALTER TABLE market_chat_messages ADD CONSTRAINT market_chat_messages_ref_market_id FOREIGN KEY (market_id) REFERENCES markets (id) ON DELETE CASCADE;
ALTER TABLE market_chat_messages ADD CONSTRAINT market_chat_messages_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
CREATE INDEX market_chat_messages_market_id_created_at_index ON market_chat_messages (market_id, created_at);