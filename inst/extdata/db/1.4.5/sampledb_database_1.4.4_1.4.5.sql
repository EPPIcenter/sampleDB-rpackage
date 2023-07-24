--- Convert back to YYYY-MM-DD ---
UPDATE "specimen" SET collection_date = date(collection_date * 86400, 'unixepoch');

INSERT OR ROLLBACK INTO version (name) VALUES ('1.4.5');
