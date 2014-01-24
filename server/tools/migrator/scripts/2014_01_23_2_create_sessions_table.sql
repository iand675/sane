-- Table: sessions

-- DROP TABLE sessions;

CREATE TABLE sessions
(
  user_id integer NOT NULL,
  session_id bytea NOT NULL,
  CONSTRAINT sessions_pkey PRIMARY KEY (user_id, session_id),
  CONSTRAINT sessions_user_id_fkey FOREIGN KEY (user_id)
      REFERENCES users (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT sessions_session_id_key UNIQUE (session_id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sessions
  OWNER TO accounts_api;

-- Index: sessions_session_id_idx

-- DROP INDEX sessions_session_id_idx;

CREATE INDEX sessions_session_id_idx
  ON sessions
  USING btree
  (session_id);


