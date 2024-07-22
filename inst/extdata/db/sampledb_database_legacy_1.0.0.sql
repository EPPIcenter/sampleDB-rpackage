CREATE TABLE "bag" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"bag_name"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--
CREATE TABLE "box" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"box_name"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--
CREATE TABLE "NEW_location" (
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
); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "NEW_location" (created, last_updated, id, location_name, location_type, level_I, level_II)
SELECT created, last_updated, id, description, "Micronix", description, description
FROM "location"; --! COMMAND_END !--

DROP TABLE location; --! COMMAND_END !-- 
ALTER TABLE "NEW_location" RENAME TO "location"; --! COMMAND_END !--

CREATE TABLE "NEW_matrix_plate" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"plate_name"	VARCHAR NOT NULL UNIQUE, plate_barcode TEXT,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "NEW_matrix_plate" (created, last_updated, id, location_id, plate_name)
SELECT created, last_updated, id, location_id, uid
FROM "matrix_plate"; --! COMMAND_END !--

DROP TABLE matrix_plate; --! COMMAND_END !--
ALTER TABLE "NEW_matrix_plate" RENAME TO "matrix_plate"; --! COMMAND_END !--


DROP INDEX ix_matrix_tube_plate_id; --! COMMAND_END !--
DROP INDEX ix_matrix_tube_barcode; --! COMMAND_END !--
CREATE TABLE "paper" (
	"id"	INTEGER NOT NULL,
	"bag_id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "bag"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--
CREATE TABLE "rdt" (
	"id"	INTEGER NOT NULL,
	"bag_id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "bag"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--
CREATE TABLE "sequencing_files" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"sequencing_metadata_id"	VARCHAR NOT NULL,
	"file_hash"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("sequencing_metadata_id") REFERENCES "sequencing_metadata"("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--
DROP INDEX ix_specimen_study_subject_id; --! COMMAND_END !--
DROP INDEX ix_specimen_type_label; --! COMMAND_END !--

CREATE TABLE "state" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"name"	VARCHAR NOT NULL UNIQUE
); --! COMMAND_END !--
CREATE TABLE "state_status_relationship" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"status_id"	INTEGER NOT NULL,
	"state_id" INTEGER NOT NULL,
	"default"  INTEGER NOT NULL,

	FOREIGN KEY("status_id") REFERENCES "status"("id"),
	FOREIGN KEY("state_id") REFERENCES "state"("id")
); --! COMMAND_END !--
CREATE TABLE "status" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"name"	VARCHAR NOT NULL UNIQUE
); --! COMMAND_END !--

INSERT OR IGNORE INTO "status" ("name") VALUES ("In Use"), ("Exhausted"), ("Bad"), ("Missing"); --! COMMAND_END !--
INSERT OR IGNORE INTO "state" ("name") VALUES ("Active"), ("Archived"); --! COMMAND_END !--
INSERT OR IGNORE INTO "state_status_relationship" ("status_id", "state_id", "default") 
VALUES 
	(1, 1, TRUE), 
	(2, 2, FALSE),
	(3, 2, FALSE), 
	(4, 2, FALSE); --! COMMAND_END !-- 	

CREATE TABLE "NEW_storage_container" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"specimen_id"	INTEGER NOT NULL,
	"type"	VARCHAR(255),
	"comment" TEXT DEFAULT NULL,
	"state_id"	INTEGER NOT NULL,
	"status_id"	INTEGER NOT NULL,

	FOREIGN KEY("state_id") REFERENCES "state"("id"),
	FOREIGN KEY("status_id") REFERENCES "status"("id"),
	FOREIGN KEY("specimen_id") REFERENCES "specimen"("id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "NEW_storage_container" (created, last_updated, id, specimen_id, type, comment, state_id, status_id)
SELECT created, last_updated, id, specimen_id, "Micronix", comments, 1, 1
FROM "storage_container"; --! COMMAND_END !--

DROP TABLE storage_container; --! COMMAND_END !-- -- due to schema mismatch
ALTER TABLE "NEW_storage_container" RENAME TO "storage_container"; --! COMMAND_END !--


--- specimen ---

CREATE TABLE IF NOT EXISTS "NEW_specimen" (
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
); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "NEW_specimen" (created, last_updated, id, study_subject_id, specimen_type_id, collection_date)
SELECT created, last_updated, id, study_subject_id, specimen_type_id, collection_date
FROM "specimen"; --! COMMAND_END !--

DROP TABLE specimen; --! COMMAND_END !-- -- due to schema mismatch
ALTER TABLE "NEW_specimen" RENAME TO "specimen"; --! COMMAND_END !--


--- study ---

CREATE TABLE "NEW_study" (
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
); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "NEW_study" (created, last_updated, id, title, description, short_code, is_longitudinal, lead_person)
SELECT created, last_updated, id, title, description, short_code, is_longitudinal, lead_person
FROM "study"; --! COMMAND_END !--

DROP TABLE study; --! COMMAND_END !-- -- due to schema mismatch
ALTER TABLE "NEW_study" RENAME TO "study"; --! COMMAND_END !--


--- study subject ---

CREATE TABLE "NEW_study_subject" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"study_id"	INTEGER NOT NULL,
	"subject"	VARCHAR NOT NULL,

	FOREIGN KEY("study_id") REFERENCES "study"("id"),
	CONSTRAINT "study_subject_study_uc" UNIQUE("subject","study_id"),
	PRIMARY KEY("id")
); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "NEW_study_subject" (id, created, last_updated, study_id, subject)
SELECT id, created, last_updated, study_id, uid
FROM "study_subject"; --! COMMAND_END !--

DROP TABLE study_subject; --! COMMAND_END !-- -- due to schema mismatch
ALTER TABLE "NEW_study_subject" RENAME TO "study_subject"; --! COMMAND_END !--



CREATE TABLE "tube" (
	"id"	INTEGER NOT NULL,
	"box_id"	INTEGER NOT NULL,
	"box_position"	VARCHAR NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("box_id") REFERENCES "box"("id"),
	PRIMARY KEY("id"),
	CONSTRAINT "box_position_plate_uc" UNIQUE("box_position","box_id")
); --! COMMAND_END !--

CREATE VIEW IF NOT EXISTS view_archive_statuses
AS 
SELECT status.id, status.name FROM state_status_relationship AS ssr
INNER JOIN state ON state.id = ssr.state_id
INNER JOIN status ON status.id = ssr.status_id
WHERE state.name = "Archived"; --! COMMAND_END !--

CREATE TABLE "version" (
	"name"	VARCHAR NOT NULL
); --! COMMAND_END !--
INSERT OR IGNORE INTO "version" ("name") VALUES ("1.0.0"); --! COMMAND_END !--

