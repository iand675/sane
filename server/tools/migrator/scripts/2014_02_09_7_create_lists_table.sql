-- Table: lists

-- DROP TABLE lists;

CREATE TABLE lists
(
  id serial NOT NULL,
  title text NOT NULL,
  icon text NOT NULL,
  owner integer NOT NULL,
  CONSTRAINT lists_pkey PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE lists
  OWNER TO accounts_api;

-- Index: lists_owner_idx

-- DROP INDEX lists_owner_idx;

CREATE INDEX lists_owner_idx
  ON lists
  USING btree
  (owner);


