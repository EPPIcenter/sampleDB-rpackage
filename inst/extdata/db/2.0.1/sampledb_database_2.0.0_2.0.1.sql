--- Storage Container ---

CREATE TABLE IF NOT EXISTS "NEW_storage_container" (
	"created"	DATETIME NOT NULL DEFAULT current_timestamp,
	"last_updated"	DATETIME NOT NULL DEFAULT current_timestamp,

	"id"	INTEGER NOT NULL,
	"specimen_id"	INTEGER NOT NULL,
	"comment" TEXT DEFAULT NULL,
	"state_id"	INTEGER NOT NULL,
	"status_id"	INTEGER NOT NULL,

	FOREIGN KEY("state_id") REFERENCES "state"("id"),
	FOREIGN KEY("status_id") REFERENCES "status"("id"),
	FOREIGN KEY("specimen_id") REFERENCES "specimen"("id"),
	PRIMARY KEY("id")
);

INSERT OR ROLLBACK INTO "NEW_storage_container" (created, last_updated, id, specimen_id, comment, state_id, status_id)
SELECT created, last_updated, id, specimen_id, comment, state_id, status_id
FROM "storage_container";

DROP TABLE IF EXISTS "storage_container";
ALTER TABLE "NEW_storage_container" RENAME TO "storage_container";

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('2.0.1');