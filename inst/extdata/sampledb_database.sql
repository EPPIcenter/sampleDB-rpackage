PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "specimen" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"study_subject_id"	INTEGER NOT NULL,
	"specimen_type_id"	INTEGER NOT NULL,
	"collection_date"	DATE,

	FOREIGN KEY("study_subject_id") REFERENCES "study_subject"("id"),
	FOREIGN KEY("specimen_type_id") REFERENCES "specimen_type"("id"),
	CONSTRAINT "specimen_collection_date_uc" UNIQUE("study_subject_id","specimen_type_id","collection_date"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "storage_container" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"specimen_id"	INTEGER NOT NULL,
	"type"	VARCHAR(255),
	"comment"	VARCHAR(255),
	"state_id"	INTEGER NOT NULL,
	"status_id"	INTEGER NOT NULL,

	FOREIGN KEY("state_id") REFERENCES "state"("id"),
	FOREIGN KEY("status_id") REFERENCES "status"("id"),
	FOREIGN KEY("specimen_id") REFERENCES "specimen"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "study_subject" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"study_id"	INTEGER NOT NULL,
	"subject"	VARCHAR NOT NULL,

	FOREIGN KEY("study_id") REFERENCES "study"("id"),
	CONSTRAINT "study_subject_study_uc" UNIQUE("subject","study_id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "matrix_plate" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"plate_name"	VARCHAR NOT NULL UNIQUE, plate_barcode TEXT,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "matrix_tube" (
	"id"	INTEGER NOT NULL,
	"plate_id"	INTEGER NOT NULL,
	"barcode"	VARCHAR NOT NULL UNIQUE,
	"well_position"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("plate_id") REFERENCES "matrix_plate"("id"),
	PRIMARY KEY("id"),
	CONSTRAINT "well_position_plate_uc" UNIQUE("well_position","plate_id")
);
CREATE TABLE IF NOT EXISTS "box" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"box_name"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "tube" (
	"id"	INTEGER NOT NULL,
	"box_id"	INTEGER NOT NULL,
	"box_position"	VARCHAR NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("box_id") REFERENCES "box"("id"),
	PRIMARY KEY("id"),
	CONSTRAINT "box_position_plate_uc" UNIQUE("box_position","box_id")
);
CREATE TABLE IF NOT EXISTS "bag" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"bag_name"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "rdt" (
	"id"	INTEGER NOT NULL,
	"bag_id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "bag"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "paper" (
	"id"	INTEGER NOT NULL,
	"bag_id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "bag"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "sequencing_files" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"sequencing_metadata_id"	VARCHAR NOT NULL,
	"file_hash"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("sequencing_metadata_id") REFERENCES "sequencing_metadata"("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "location" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_name"	VARCHAR NOT NULL,
	"location_type"	VARCHAR NOT NULL,
	"level_I"	VARCHAR NOT NULL,
	"level_II"	VARCHAR NOT NULL,
	"level_III"	VARCHAR,

	CONSTRAINT "location_uc" UNIQUE("location_name","level_I","level_II"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "specimen_type" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL UNIQUE,

	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "study" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"title"	VARCHAR NOT NULL UNIQUE,
	"description"	VARCHAR,
	"short_code"	VARCHAR NOT NULL UNIQUE,
	"is_longitudinal"	BOOLEAN NOT NULL,
	"lead_person"	VARCHAR NOT NULL,
	CHECK("is_longitudinal" IN (0, 1)),

	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "sequencing_metadata" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"metadata"	VARCHAR NOT NULL,

	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "version" (
	"name"	VARCHAR NOT NULL
);

CREATE TABLE IF NOT EXISTS "status" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"name"	VARCHAR NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS "state" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"name"	VARCHAR NOT NULL UNIQUE
);

INSERT OR IGNORE INTO "status" ("name") VALUES ("In Use"), ("Exhausted"), ("Separated"), ("Missing");
INSERT OR IGNORE INTO "state" ("name") VALUES ("Active"), ("Archived");
INSERT OR IGNORE INTO "version" ("name") VALUES ("1.0.0");

COMMIT;

