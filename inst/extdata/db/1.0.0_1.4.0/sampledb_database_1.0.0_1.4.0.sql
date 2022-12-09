BEGIN TRANSACTION;

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

INSERT OR ABORT INTO "cryovial_box" (created, last_updated, id, location_id, name)
SELECT created, last_updated, id, location_id, box_name
FROM "box";

DROP TABLE IF EXISTS "box";

--- Cryovial Tube ---

ALTER TABLE "tube" RENAME COLUMN "label" TO "barcode";
ALTER TABLE "tube" RENAME COLUMN "box_id" TO "manifest_id";
ALTER TABLE "tube" RENAME TO "cryovial_tube";


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

INSERT OR ABORT INTO "micronix_plate" (created, last_updated, id, location_id, name)
SELECT created, last_updated, id, location_id, plate_name
FROM "matrix_plate";

DROP TABLE IF EXISTS "matrix_plate";

--- Micronix Tube ---

CREATE TABLE IF NOT EXISTS "micronix_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") == 3 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "micronix_plate"("id"),

	CONSTRAINT "matrix_tube_position_plate_uc" UNIQUE("position", "manifest_id")
);

INSERT OR ABORT INTO "micronix_tube" (id, manifest_id, barcode, position)
SELECT id, plate_id, barcode, well_position
FROM "matrix_tube";

DROP TABLE IF EXISTS "matrix_tube";

--- Specimen Type ---

ALTER TABLE "specimen_type" RENAME COLUMN "label" TO "name";

--- Storage Container ---

ALTER TABLE "storage_container" ADD COLUMN "derived_storage_container_id";

--- Study Subject ---

ALTER TABLE "study_subject" RENAME COLUMN "subject" TO "name";

DROP TABLE IF EXISTS bag;
DROP TABLE IF EXISTS paper;
DROP TABLE IF EXISTS rdt;
DROP TABLE IF EXISTS sequencing_files;
DROP TABLE IF EXISTS sqlite_sequence;


--- update database version ---
UPDATE version SET name='1.4.0' WHERE rowid=1;
COMMIT;