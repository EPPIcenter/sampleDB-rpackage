BEGIN TRANSACTION;
DROP TABLE bag;
DROP TABLE box;
CREATE TABLE "cryovial_box" (
	"created"		DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"			INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"barcode"		VARCHAR DEFAULT NULL UNIQUE,

	PRIMARY KEY("id"),
	FOREIGN KEY("location_id") REFERENCES "location"("id")
);
CREATE TABLE "cryovial_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") == 3 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "cryovial_box"("id"),

	CONSTRAINT "box_index_plate_uc" UNIQUE("position", "manifest_id")
	
);
DROP TABLE matrix_plate;
DROP TABLE matrix_tube;
CREATE TABLE "micronix_plate" (
	"created"		DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"			INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE, 
	"barcode"		VARCHAR DEFAULT NULL UNIQUE,

	PRIMARY KEY("id"),
	FOREIGN KEY("location_id") REFERENCES "location"("id")
);
CREATE TABLE "micronix_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") == 3 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "micronix_plate"("id"),

	CONSTRAINT "matrix_tube_position_plate_uc" UNIQUE("position", "manifest_id")
);
DROP TABLE paper;
DROP TABLE rdt;
DROP TABLE sequencing_files;
DROP TABLE specimen_type; -- due to schema mismatch
CREATE TABLE "specimen_type" (
	"created"			DATETIME NOT NULL,
	"last_updated"		DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"name"				VARCHAR NOT NULL UNIQUE,

	PRIMARY KEY("id")
);
DROP TABLE sqlite_sequence;
ALTER TABLE storage_container ADD COLUMN derived_storage_container_id;
DROP TABLE study_subject; -- due to schema mismatch
CREATE TABLE "study_subject" (
	"created"			DATETIME NOT NULL,
	"last_updated"		DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"study_id"			INTEGER NOT NULL,
	"name"				VARCHAR NOT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("study_id") REFERENCES "study"("id"),

	CONSTRAINT "study_subject_study_uc" UNIQUE("name","study_id")
);
DROP TABLE tube;
UPDATE version SET name='1.4.0' WHERE rowid=1;
COMMIT;
