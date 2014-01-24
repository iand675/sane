-- Table: users

-- DROP TABLE users;

CREATE TABLE users
(
  id serial NOT NULL,
  username text NOT NULL,
  name text NOT NULL,
  avatar text,
  password_hash bytea NOT NULL,
  email text NOT NULL,
  cellphone text,
  stripe_token text NOT NULL,
  CONSTRAINT users_pkey PRIMARY KEY (id),
  CONSTRAINT users_username_key UNIQUE (username)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE users
  OWNER TO accounts_api;

-- Index: users_email_idx

-- DROP INDEX users_email_idx;

CREATE INDEX users_email_idx
  ON users
  USING btree
  (email COLLATE pg_catalog."default");

-- Index: users_username_idx

-- DROP INDEX users_username_idx;

CREATE INDEX users_username_idx
  ON users
  USING btree
  (username COLLATE pg_catalog."default");


