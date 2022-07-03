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
	"exhausted"	BOOLEAN NOT NULL,

	FOREIGN KEY("specimen_id") REFERENCES "specimen"("id"),
	CHECK("exhausted" IN (0, 1)),
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
CREATE TABLE IF NOT EXISTS "deleted_matrix_tube" (
"id"    INTEGER NOT NULL,
"plate_id"      INTEGER NOT NULL,
"barcode"       VARCHAR NOT NULL,
"well_position" VARCHAR NOT NULL,
"deleted_datetime" TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS "deleted_matrix_plate" (
"id"    INTEGER NOT NULL,
"location_id"    INTEGER NOT NULL,
"plate_name"    VARCHAR NOT NULL UNIQUE,
"plate_barcode" TEXT,
"deleted_datetime" TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS "deleted_storage_container" (
"id"    INTEGER NOT NULL,
"specimen_id"    INTEGER NOT NULL,
"type"    VARCHAR(255),
"exhausted"    BOOLEAN NOT NULL,
"deleted_datetime" TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS "deleted_specimen" (
"id"    INTEGER NOT NULL,
"study_subject_id"    INTEGER NOT NULL,
"specimen_type_id"    INTEGER NOT NULL,
"collection_date"    DATE,
"deleted_datetime"    TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS "deleted_study_subject" (
"id"    INTEGER NOT NULL,
"study_id"    INTEGER NOT NULL,
"subject"    VARCHAR NOT NULL,
"deleted_datetime"    TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS "deleted_study" (
"id"    INTEGER NOT NULL,
"title"    VARCHAR NOT NULL UNIQUE,
"description"    VARCHAR,
"short_code"    VARCHAR NOT NULL UNIQUE,
"is_longitudinal"    BOOLEAN NOT NULL,
"lead_person"    VARCHAR NOT NULL,
"deleted_datetime" TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS "deleted_specimen_type" (
"id"    INTEGER NOT NULL,
"label"    VARCHAR NOT NULL UNIQUE,
"deleted_datetime" TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS "deleted_location" (
"id"    INTEGER NOT NULL,
    "location_name"    VARCHAR NOT NULL,
    "location_type"    VARCHAR NOT NULL,
    "level_I"    VARCHAR NOT NULL,
    "level_II"    VARCHAR NOT NULL,
    "level_III"    VARCHAR,
"deleted_datetime" TEXT NOT NULL);
CREATE TRIGGER matrix_tube_insert AFTER DELETE ON matrix_tube
BEGIN
INSERT INTO deleted_matrix_tube(id, plate_id, barcode, well_position, deleted_datetime)
VALUES(OLD.id, OLD.plate_id, OLD.barcode, OLD.well_position, datetime('now', 'localtime'));
END;
CREATE TRIGGER matrix_plate_insert AFTER DELETE ON matrix_plate
BEGIN
INSERT INTO deleted_matrix_plate(id, location_id, plate_name, plate_barcode, deleted_datetime)
VALUES(OLD.id, OLD.location_id, OLD.plate_name, OLD.plate_barcode, datetime('now', 'localtime'));
END;
CREATE TRIGGER storage_container_insert AFTER DELETE ON storage_container
BEGIN
INSERT INTO deleted_storage_container(id, specimen_id, type, exhausted, deleted_datetime)
VALUES(OLD.id, OLD.specimen_id, OLD.type, OLD.exhausted, datetime('now', 'localtime'));
END;
CREATE TRIGGER specimen_insert AFTER DELETE ON specimen
BEGIN
INSERT INTO deleted_specimen(id, study_subject_id, specimen_type_id, collection_date, deleted_datetime)
VALUES(OLD.id, OLD.study_subject_id, OLD.specimen_type_id, OLD.collection_date, datetime('now', 'localtime'));
END;
CREATE TRIGGER study_subject_insert AFTER DELETE ON study_subject
BEGIN
INSERT INTO deleted_study_subject(id, study_id, subject, deleted_datetime)
VALUES(OLD.id, OLD.study_id, OLD.subject, datetime('now', 'localtime'));
END;
CREATE TRIGGER study_insert AFTER DELETE ON study
BEGIN
INSERT INTO deleted_study(id, title, description, short_code, is_longitudinal, lead_person, deleted_datetime)
VALUES(OLD.id, OLD.title, OLD.description, OLD.short_code, OLD.is_longitudinal, OLD.lead_person, datetime('now', 'localtime'));
END;
CREATE TRIGGER specimen_type_insert AFTER DELETE ON specimen_type
BEGIN
INSERT INTO deleted_specimen_type(id, label, deleted_datetime)
VALUES(OLD.id, OLD.label, datetime('now', 'localtime'));
END;
CREATE TRIGGER location_insert AFTER DELETE ON location
BEGIN
INSERT INTO deleted_location(id, location_name, location_type, level_I, level_II, level_III, deleted_datetime)
VALUES(OLD.id, OLD.location_name, OLD.location_type, OLD.level_I, OLD.level_II, OLD.level_III, datetime('now', 'localtime'));
END;
COMMIT;
