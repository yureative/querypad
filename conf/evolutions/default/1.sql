# --- !Ups

CREATE SEQUENCE query_history_seq;
CREATE TABLE query_history (
    id         INTEGER     DEFAULT nextval('query_history_seq') PRIMARY KEY,
    name       VARCHAR(50) NOT NULL,
    sql        TEXT        NOT NULL,
    updated_at TIMESTAMP   NOT NULL
);
CREATE INDEX query_history_idx1 ON query_history(updated_at);

# --- !Downs
                         
DROP TABLE query_history;
DROP SEQUENCE query_history_seq;
