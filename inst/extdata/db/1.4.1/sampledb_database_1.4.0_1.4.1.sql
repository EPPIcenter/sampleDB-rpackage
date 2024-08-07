INSERT OR ROLLBACK INTO "sample_type" (name)
VALUES
	("DBS"); --! COMMAND_END !--

CREATE TABLE IF NOT EXISTS  "dbs_paper"  (
	"created"		DATETIME NOT NULL DEFAULT current_timestamp,
	"last_updated"	DATETIME NOT NULL DEFAULT current_timestamp,

	"id"			INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"barcode"		VARCHAR DEFAULT NULL UNIQUE,

	PRIMARY KEY("id"),
	FOREIGN KEY("location_id") REFERENCES "location"("id")
); --! COMMAND_END !--


CREATE TABLE IF NOT EXISTS "dbs_spot" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"position"		VARCHAR CHECK(length("position") > 1 OR "position" IS NULL),
    "strain"		VARCHAR NOT NULL,
    "0.05"			INTEGER DEFAULT 0 CHECK("0.05" >= 0),
    "0.1"			INTEGER DEFAULT 0 CHECK("0.1" >= 0),
    "1"				INTEGER DEFAULT 0 CHECK("1" >= 0),
    "10"			INTEGER DEFAULT 0 CHECK("10" >= 0),
    "100"			INTEGER DEFAULT 0 CHECK("100" >= 0),
    "1k"			INTEGER DEFAULT 0 CHECK("1k" >= 0),
    "10k"			INTEGER DEFAULT 0 CHECK("10k" >= 0),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "dbs_paper"("id"),

	CONSTRAINT "cryovial_tube_position_manifest_id_uc" UNIQUE("position", "manifest_id")
); --! COMMAND_END !--


--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('1.4.1'); --! COMMAND_END !--
