CREATE TABLE IF NOT EXISTS "static_well" (
	"id"	INTEGER NOT NULL,
	"plate_id"	INTEGER NOT NULL,
	"well_position"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("plate_id") REFERENCES "matrix_plate"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--


INSERT OR IGNORE INTO "version" ("name") VALUES ("2.3.0"); --! COMMAND_END !--
