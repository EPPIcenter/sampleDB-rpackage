--- Storage Type ---

CREATE TABLE IF NOT EXISTS "storage_type" (
	"name"			VARCHAR NOT NULL UNIQUE,
	"description"	TEXT,

	PRIMARY KEY("name")
);

INSERT OR ROLLBACK INTO "storage_type" (name)
VALUES
	("Micronix"),
	("Cryovial");

--- Location ---

CREATE TABLE IF NOT EXISTS "NEW_location" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"name"				VARCHAR NOT NULL,
	"storage_type"		VARCHAR NOT NULL,
	"description" 		TEXT,
	"level_I"			VARCHAR NOT NULL,
	"level_II"			VARCHAR NOT NULL,
	"level_III"			VARCHAR,


	PRIMARY KEY("id"),
	FOREIGN KEY("storage_type") REFERENCES "storage_type"("name")

);

INSERT OR ROLLBACK INTO "NEW_location" (created, last_updated, id, name, storage_type, description, level_I, level_II, level_III)
SELECT created, last_updated, id, location_name, "Micronix", location_type, level_I, level_II, level_III
FROM "location";

--- Cryovial Box ---

CREATE TABLE IF NOT EXISTS  "cryovial_box"  (
	"created"		DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

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
	FOREIGN KEY("manifest_id") REFERENCES "micronix_plate"("id"),

	CONSTRAINT "cryovial_tube_position_manifest_id_uc" UNIQUE("position", "manifest_id")
);

--- Micronix Plate ---

CREATE TABLE IF NOT EXISTS "micronix_plate" (
	"created"		DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

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

ALTER TABLE "storage_container" ADD COLUMN "derived_storage_container_id";

--- Study Subject ---

ALTER TABLE "study_subject" RENAME COLUMN "subject" TO "name";

DROP TABLE IF EXISTS "location";
ALTER TABLE "NEW_location" RENAME TO "location";

--- Sample Types ---

DROP TABLE IF EXISTS "matrix_plate";
DROP TABLE IF EXISTS "box";
DROP TABLE IF EXISTS "bag";
DROP TABLE IF EXISTS "paper";
DROP TABLE IF EXISTS "rdt";
DROP TABLE IF EXISTS "sequencing_files";

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('1.4.0');