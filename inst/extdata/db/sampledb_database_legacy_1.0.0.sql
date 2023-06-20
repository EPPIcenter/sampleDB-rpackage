CREATE TABLE "bag" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"bag_name"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE "box" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"box_name"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);
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
);

INSERT OR ROLLBACK INTO "NEW_location" (created, last_updated, id, location_name, location_type, level_I, level_II)
SELECT created, last_updated, id, description, "Micronix", description, description
FROM "location";

DROP TABLE location; 
ALTER TABLE "NEW_location" RENAME TO "location";

CREATE TABLE "NEW_matrix_plate" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"plate_name"	VARCHAR NOT NULL UNIQUE, plate_barcode TEXT,

	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);

INSERT OR ROLLBACK INTO "NEW_matrix_plate" (created, last_updated, id, location_id, plate_name)
SELECT created, last_updated, id, location_id, uid
FROM "matrix_plate";

DROP TABLE matrix_plate;
ALTER TABLE "NEW_matrix_plate" RENAME TO "matrix_plate";


DROP INDEX ix_matrix_tube_plate_id;
DROP INDEX ix_matrix_tube_barcode;
CREATE TABLE "paper" (
	"id"	INTEGER NOT NULL,
	"bag_id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "bag"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE "rdt" (
	"id"	INTEGER NOT NULL,
	"bag_id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "bag"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE "sequencing_files" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"	INTEGER NOT NULL,
	"sequencing_metadata_id"	VARCHAR NOT NULL,
	"file_hash"	VARCHAR NOT NULL UNIQUE,

	FOREIGN KEY("sequencing_metadata_id") REFERENCES "sequencing_metadata"("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	PRIMARY KEY("id")
);
DROP INDEX ix_specimen_study_subject_id;
DROP INDEX ix_specimen_type_label;

CREATE TABLE "state" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"name"	VARCHAR NOT NULL UNIQUE
);
CREATE TABLE "state_status_relationship" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"status_id"	INTEGER NOT NULL,
	"state_id" INTEGER NOT NULL,
	"default"  INTEGER NOT NULL,

	FOREIGN KEY("status_id") REFERENCES "status"("id"),
	FOREIGN KEY("state_id") REFERENCES "state"("id")
);
CREATE TABLE "status" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"name"	VARCHAR NOT NULL UNIQUE
);

INSERT OR IGNORE INTO "status" ("name") VALUES ("In Use"), ("Exhausted"), ("Bad"), ("Missing");
INSERT OR IGNORE INTO "state" ("name") VALUES ("Active"), ("Archived");
INSERT OR IGNORE INTO "state_status_relationship" ("status_id", "state_id", "default") 
VALUES 
	(1, 1, TRUE), 
	(2, 2, FALSE),
	(3, 2, FALSE), 
	(4, 2, FALSE); 	

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
);

INSERT OR ROLLBACK INTO "NEW_storage_container" (created, last_updated, id, specimen_id, type, comment, state_id, status_id)
SELECT created, last_updated, id, specimen_id, "Micronix", comments, 1, 1
FROM "storage_container";

DROP TABLE storage_container; -- due to schema mismatch
ALTER TABLE "NEW_storage_container" RENAME TO "storage_container";


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
);

INSERT OR ROLLBACK INTO "NEW_specimen" (created, last_updated, id, study_subject_id, specimen_type_id, collection_date)
SELECT created, last_updated, id, study_subject_id, specimen_type_id, collection_date
FROM "specimen";

DROP TABLE specimen; -- due to schema mismatch
ALTER TABLE "NEW_specimen" RENAME TO "specimen";


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
);

INSERT OR ROLLBACK INTO "NEW_study" (created, last_updated, id, title, description, short_code, is_longitudinal, lead_person)
SELECT created, last_updated, id, title, description, short_code, is_longitudinal, lead_person
FROM "study";

DROP TABLE study; -- due to schema mismatch
ALTER TABLE "NEW_study" RENAME TO "study";


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
);

INSERT OR ROLLBACK INTO "NEW_study_subject" (id, created, last_updated, study_id, subject)
SELECT id, created, last_updated, study_id, uid
FROM "study_subject";

DROP TABLE study_subject; -- due to schema mismatch
ALTER TABLE "NEW_study_subject" RENAME TO "study_subject";


DROP TABLE tbl1;
CREATE TABLE "tube" (
	"id"	INTEGER NOT NULL,
	"box_id"	INTEGER NOT NULL,
	"box_position"	VARCHAR NOT NULL,
	"label"	VARCHAR NOT NULL,

	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("box_id") REFERENCES "box"("id"),
	PRIMARY KEY("id"),
	CONSTRAINT "box_position_plate_uc" UNIQUE("box_position","box_id")
);

CREATE VIEW IF NOT EXISTS view_archive_statuses
AS 
SELECT status.id, status.name FROM state_status_relationship AS ssr
INNER JOIN state ON state.id = ssr.state_id
INNER JOIN status ON status.id = ssr.status_id
WHERE state.name = "Archived";

CREATE TABLE "version" (
	"name"	VARCHAR NOT NULL
);
INSERT OR IGNORE INTO "version" ("name") VALUES ("1.0.0");

