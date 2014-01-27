-- Column: facebook_token

-- ALTER TABLE users DROP COLUMN facebook_token;

ALTER TABLE users ADD COLUMN facebook_token text;

-- Column: facebook_expiration

-- ALTER TABLE users DROP COLUMN facebook_expiration;

ALTER TABLE users ADD COLUMN facebook_expiration timestamp without time zone;

-- Column: facebook_id

-- ALTER TABLE users DROP COLUMN facebook_id;

ALTER TABLE users ADD COLUMN facebook_id text;

