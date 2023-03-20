--- Sample Type ---

CREATE TABLE IF NOT EXISTS "sample_type" (
	"id"			INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"description"	TEXT,

	PRIMARY KEY("id")
);

INSERT OR ROLLBACK INTO "sample_type" (name)
VALUES
	("Micronix"),
	("Cryovial");

--- Storage Type ---

CREATE TABLE IF NOT EXISTS "storage_type" (
	"id"			INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"description"	TEXT,

	PRIMARY KEY("id")
);

INSERT OR ROLLBACK INTO "storage_type" (name)
VALUES
	("Minus20"),
	("Minus80");

--- Location ---

CREATE TABLE IF NOT EXISTS "NEW_location" (
	"created"	DATETIME NOT NULL DEFAULT current_timestamp,
	"last_updated"	DATETIME NOT NULL DEFAULT current_timestamp,

	"id"				INTEGER NOT NULL,
	"name"				VARCHAR NOT NULL,
	"storage_type_id"	VARCHAR NOT NULL,
	"description" 		TEXT,
	"level_I"			VARCHAR NOT NULL,
	"level_II"			VARCHAR NOT NULL,
	"level_III"			VARCHAR,


	PRIMARY KEY("id"),
	FOREIGN KEY("storage_type_id") REFERENCES "storage_type_id"("id")

);

INSERT OR ROLLBACK INTO "NEW_location" (created, last_updated, id, name, storage_type_id, description, level_I, level_II, level_III)
SELECT created, last_updated, id, location_name, 1, location_type, level_I, level_II, level_III
FROM "location";

--- Cryovial Box ---

CREATE TABLE IF NOT EXISTS  "cryovial_box"  (
	"created"		DATETIME NOT NULL DEFAULT current_timestamp,
	"last_updated"	DATETIME NOT NULL DEFAULT current_timestamp,

	"id"			INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"barcode"		VARCHAR DEFAULT NULL UNIQUE,

	PRIMARY KEY("id"),
	FOREIGN KEY("location_id") REFERENCES "location"("id")
);

INSERT OR ROLLBACK INTO "cryovial_box" (created, last_updated, id, location_id, name)
SELECT created, last_updated, id, location_id, box_name
FROM "box";

--- Cryovial Tube ---

CREATE TABLE IF NOT EXISTS "cryovial_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") > 1 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "cryovial_box"("id"),

	CONSTRAINT "cryovial_tube_position_manifest_id_uc" UNIQUE("position", "manifest_id")
);

--- Micronix Plate ---

CREATE TABLE IF NOT EXISTS "micronix_plate" (
	"created"		DATETIME NOT NULL DEFAULT current_timestamp,
	"last_updated"	DATETIME NOT NULL DEFAULT current_timestamp,

	"id"			INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE, 
	"barcode"		VARCHAR DEFAULT NULL UNIQUE,

	PRIMARY KEY("id"),
	FOREIGN KEY("location_id") REFERENCES "location"("id")
);

INSERT OR ROLLBACK INTO "micronix_plate" (created, last_updated, id, location_id, name)
SELECT created, last_updated, id, location_id, plate_name
FROM "matrix_plate";

--- Micronix Tube ---

CREATE TABLE IF NOT EXISTS "micronix_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") > 1 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "micronix_plate"("id"),

	CONSTRAINT "micronix_tube_position_manifest_id_uc" UNIQUE("position", "manifest_id")
);

INSERT OR ROLLBACK INTO "micronix_tube" (id, manifest_id, barcode, position)
SELECT id, plate_id, barcode, NULLIF(well_position, "NA") AS position
FROM "matrix_tube";

DROP TABLE IF EXISTS "matrix_tube";

--- Specimen Type ---

ALTER TABLE "specimen_type" RENAME COLUMN "label" TO "name";

--- Storage Container ---

CREATE TABLE IF NOT EXISTS "NEW_storage_container" (
	"created"	DATETIME NOT NULL DEFAULT current_timestamp,
	"last_updated"	DATETIME NOT NULL DEFAULT current_timestamp,

	"id"	INTEGER NOT NULL,
	"specimen_id"	INTEGER NOT NULL,
	"sample_type_id" INTEGER NOT NULL,
	"comment" TEXT DEFAULT NULL,
	"state_id"	INTEGER NOT NULL,
	"status_id"	INTEGER NOT NULL,

	FOREIGN KEY("state_id") REFERENCES "state"("id"),
	FOREIGN KEY("status_id") REFERENCES "status"("id"),
	FOREIGN KEY("specimen_id") REFERENCES "specimen"("id"),
	FOREIGN KEY("sample_type_id") REFERENCES "sample_type"("id"),
	PRIMARY KEY("id")
);

INSERT OR ROLLBACK INTO "NEW_storage_container" (created, last_updated, id, specimen_id, sample_type_id, comment, state_id, status_id)
SELECT created, last_updated, id, specimen_id, 1, comment, state_id, status_id
FROM "storage_container";


--- Study Subject ---

ALTER TABLE "study_subject" RENAME COLUMN "subject" TO "name";

--- Drop Tables  ---

DROP TABLE IF EXISTS "location";
ALTER TABLE "NEW_location" RENAME TO "location";

DROP TABLE IF EXISTS "storage_container";
ALTER TABLE "NEW_storage_container" RENAME TO "storage_container";

DROP TABLE IF EXISTS "matrix_plate";
DROP TABLE IF EXISTS "box";
DROP TABLE IF EXISTS "bag";
DROP TABLE IF EXISTS "paper";
DROP TABLE IF EXISTS "rdt";
DROP TABLE IF EXISTS "sequencing_files";

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('1.4.0');