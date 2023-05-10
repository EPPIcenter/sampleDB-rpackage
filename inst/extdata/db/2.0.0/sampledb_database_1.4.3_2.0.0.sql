ALTER TABLE "sample_type" ADD COLUMN "parent_id" INTEGER DEFAULT NULL REFERENCES "sample_type"("id");
ALTER TABLE "dbs_spot" RENAME TO "dbs_control";

--- insert new derived sample types
INSERT OR ROLLBACK INTO "sample_type" (name, parent_id)
VALUES
	("Tube", 3),
	("Paper", 3),
	("Control", 3);

--- create the dbs bag that holds dbs paper ---
CREATE TABLE IF NOT EXISTS "dbs_bag" (
	"created"		DATETIME NOT NULL DEFAULT current_timestamp,
	"last_updated"	DATETIME NOT NULL DEFAULT current_timestamp,
	"id"			INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL,
	"barcode"		VARCHAR,
	"description"	VARCHAR,

	PRIMARY KEY("id"),
	FOREIGN KEY("location_id") REFERENCES "location"("id")
);


--- DBS Tube ---
CREATE TABLE IF NOT EXISTS "dbs_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") > 1 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "dbs_bag"("id"),

	CONSTRAINT "dbs_tube_position_manifest_id_uc" UNIQUE("position", "manifest_id")
);

--- DBS Paper ---
CREATE TABLE IF NOT EXISTS "dbs_paper" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"type"			INTEGER NOT NULL CHECK("type" == 5 OR "type" == 6),
	"barcode"		VARCHAR CHECK("type" == 5 AND "barcode" IS NOT NULL),
	"position"		VARCHAR CHECK(length("position") > 1 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "dbs_bag"("id"),

	CONSTRAINT "dbs_paper_position_manifest_id_uc" UNIQUE("position", "manifest_id")
);

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('2.0.0');
