CREATE TABLE IF NOT EXISTS "static_well" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"position"		VARCHAR,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "micronix_plate"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--


INSERT OR IGNORE INTO "version" ("name") VALUES ("2.3.0"); --! COMMAND_END !--
