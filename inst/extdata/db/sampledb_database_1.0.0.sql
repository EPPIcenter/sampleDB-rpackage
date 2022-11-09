----------------------------
-- Sample Metadata tables --
----------------------------

CREATE TABLE IF NOT EXISTS "study" (
	"created"			DATETIME NOT NULL,
	"last_updated"		DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"title"				VARCHAR NOT NULL UNIQUE,
	"description"		VARCHAR,
	"short_code"		VARCHAR NOT NULL UNIQUE,
	"is_longitudinal"	BOOLEAN NOT NULL,
	"lead_person"		VARCHAR NOT NULL,
	CHECK("is_longitudinal" IN (0, 1)),

	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "study_subject" (
	"created"			DATETIME NOT NULL,
	"last_updated"		DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"study_id"			INTEGER NOT NULL,
	"name"				VARCHAR NOT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("study_id") REFERENCES "study"("id"),

	CONSTRAINT "study_subject_study_uc" UNIQUE("name","study_id")
);

CREATE TABLE IF NOT EXISTS "specimen_type" (
	"created"			DATETIME NOT NULL,
	"last_updated"		DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"name"				VARCHAR NOT NULL UNIQUE,

	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "specimen" (
	"created"			DATETIME NOT NULL,
	"last_updated"		DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"study_subject_id"	INTEGER NOT NULL,
	"specimen_type_id"	INTEGER NOT NULL,
	"collection_date"	DATE,

	PRIMARY KEY("id"),
	FOREIGN KEY("study_subject_id") REFERENCES "study_subject"("id"),
	FOREIGN KEY("specimen_type_id") REFERENCES "specimen_type"("id"),

	CONSTRAINT "specimen_collection_date_uc" UNIQUE("study_subject_id","specimen_type_id","collection_date")
);

--------------------------------------------
-- Base data table for storage containers --
--------------------------------------------

CREATE TABLE IF NOT EXISTS "storage_container" (
	"created"			DATETIME NOT NULL,
	"last_updated"		DATETIME NOT NULL,

	"id"				INTEGER NOT NULL,
	"specimen_id"		INTEGER NOT NULL,
	"type"				VARCHAR NOT NULL,
	"comment" 			TEXT DEFAULT NULL,
	"state_id"			INTEGER NOT NULL,
	"status_id"			INTEGER NOT NULL,
	"derived_storage_container_id"	INTEGER DEFAULT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("state_id") 	REFERENCES "state"("id"),
	FOREIGN KEY("status_id") 	REFERENCES "status"("id"),
	FOREIGN KEY("specimen_id") 	REFERENCES "specimen"("id")
);

---------------------------------
-- Storage Container Manifests --
---------------------------------

CREATE TABLE IF NOT EXISTS "cryovial_box" (
	"created"		DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"			INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"barcode"		VARCHAR DEFAULT NULL UNIQUE,

	PRIMARY KEY("id"),
	FOREIGN KEY("location_id") REFERENCES "location"("id")
);


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

-----------------------------------
-- Storage Container Sub Classes --
-----------------------------------

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

CREATE TABLE IF NOT EXISTS "cryovial_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") == 3 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "cryovial_box"("id"),

	CONSTRAINT "box_index_plate_uc" UNIQUE("position", "manifest_id")
	
);

CREATE TABLE IF NOT EXISTS "location" (
	"created"		DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,

	"id"			INTEGER NOT NULL,
	"location_name"	VARCHAR NOT NULL,
	"location_type"	VARCHAR NOT NULL,
	"level_I"		VARCHAR NOT NULL,
	"level_II"		VARCHAR NOT NULL,
	"level_III"		VARCHAR,

	PRIMARY KEY("id"),

	CONSTRAINT "location_uc" UNIQUE("location_name","level_I","level_II")
);

--------------------
-- Utility Tables --
--------------------

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

CREATE TABLE IF NOT EXISTS "state_status_relationship" (
	"id"		INTEGER PRIMARY KEY AUTOINCREMENT,
	"status_id"	INTEGER NOT NULL,
	"state_id" 	INTEGER NOT NULL,
	"default"  	INTEGER NOT NULL,

	FOREIGN KEY("status_id") 	REFERENCES "status"("id"),
	FOREIGN KEY("state_id") 	REFERENCES "state"("id")
);

INSERT OR IGNORE INTO "status" ("name") VALUES ("In Use"), ("Exhausted"), ("Bad"), ("Missing");
INSERT OR IGNORE INTO "state" ("name") VALUES ("Active"), ("Archived");
INSERT OR IGNORE INTO "state_status_relationship" ("status_id", "state_id", "default") 
VALUES 
	(1, 1, TRUE), 
	(2, 2, FALSE),
	(3, 2, FALSE), 
	(4, 2, FALSE); 	

INSERT OR IGNORE INTO "version" ("name") VALUES ("1.0.0");

CREATE VIEW IF NOT EXISTS view_archive_statuses
AS 
SELECT status.id, status.name FROM state_status_relationship AS ssr
INNER JOIN state ON state.id = ssr.state_id
INNER JOIN status ON status.id = ssr.status_id
WHERE state.name = "Archived";

