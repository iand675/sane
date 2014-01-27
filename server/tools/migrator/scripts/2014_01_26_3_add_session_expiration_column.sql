-- Column: expiration

-- ALTER TABLE sessions DROP COLUMN expiration;

ALTER TABLE sessions ADD COLUMN expiration time without time zone;
ALTER TABLE sessions ALTER COLUMN expiration SET NOT NULL;

