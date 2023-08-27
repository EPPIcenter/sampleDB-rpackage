ALTER TABLE "location" RENAME COLUMN "name" TO "location_root";

ALTER TABLE "sample_type" ADD COLUMN "parent_id" INTEGER DEFAULT NULL REFERENCES "sample_type"("id");

--- Control Collection ---
CREATE TABLE IF NOT EXISTS "control_collection" (
	"id"			INTEGER NOT NULL,
	"study_id"		INTEGER NOT NULL,
	"url"			VARCHAR NOT NULL,
	"metadata"		TEXT,

	PRIMARY KEY("id")
);

--- just remove dbs ---
DROP TABLE "dbs_spot";

--- insert new derived sample types

INSERT OR ROLLBACK INTO "sample_type" (name, parent_id)
VALUES
	("Tube", 3),
	("Paper", 3);


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
	"box_id"		INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL UNIQUE,
	"position"		VARCHAR CHECK(length("position") > 1 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("box_id") REFERENCES "dbs_bag"("id"),

	CONSTRAINT "dbs_tube_position_box_id_uc" UNIQUE("position", "box_id")
);

--- DBS Paper - different than a DBS control sheet ---
CREATE TABLE IF NOT EXISTS "dbs_paper" (
	"id"			INTEGER NOT NULL,
	"bag_id"		INTEGER NOT NULL,
	"barcode"		VARCHAR NOT NULL,
	"date"			DATE NOT NULL,
	"replicates"	INTEGER NOT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "dbs_bag"("id"),

	CONSTRAINT "dbs_paper_position_bag_id_uc" UNIQUE("position", "bag_id")
);

--- Identifier for the control combination ---
CREATE TABLE IF NOT EXISTS "malaria_blood_control" (
	"id"		INTEGER NOT NULL,
	"study_subject_id" INTEGER NOT NULL,
    "composition_id" INTEGER NOT NULL,
	"density"	REAL NOT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("study_subject_id") REFERENCES "study_subject"("id")
);

--- Blood Spot Collection ---
CREATE TABLE IF NOT EXISTS "blood_spot_collection" (
	"id"			INTEGER NOT NULL,
	"malaria_blood_control_id" INTEGER NOT NULL,
	"total"			INTEGER NOT NULL CHECK ("total" > 0),
	"exhausted"		INTEGER	NOT NULL DEFAULT 0 CHECK ("exhausted" <= "total" AND "exhausted" >= 0),

	PRIMARY KEY("id"),
	FOREIGN KEY("malaria_blood_control_id") REFERENCES "malaria_blood_control"("id")
);

--- DBS Control Sheet ---
CREATE TABLE IF NOT EXISTS "dbs_control_sheet" (
	"id"			INTEGER NOT NULL,
	"dbs_bag_id" 	INTEGER NOT NULL,
	"label"			VARCHAR NOT NULL,
	"replicates"	INTEGER NOT NULL DEFAULT 1,

	PRIMARY KEY("id"),
	FOREIGN KEY("dbs_bag_id") REFERENCES "dbs_bag"("id"),
	CONSTRAINT "dbs_control_sheet_bag_id_uc" UNIQUE("label", "dbs_bag_id")
);

--- Junction Table ---
CREATE TABLE IF NOT EXISTS "blood_spot_collection_dbs_control_sheet" (
	"id"			INTEGER NOT NULL,
	"blood_spot_collection_id" INTEGER NOT NULL,
	"dbs_control_sheet_id"	INTEGER NOT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("blood_spot_collection_id") REFERENCES "blood_spot_collection"("id"),
	FOREIGN KEY("dbs_control_sheet_id") REFERENCES "dbs_control_sheet"("id")
);

--- Table of strains that can be used with controls ---
CREATE TABLE IF NOT EXISTS "strain" (
	"id"			INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"description"	VARCHAR,

	PRIMARY KEY("id")
);

--- Control composition definition table --- 
CREATE TABLE IF NOT EXISTS "composition_strain" (
	"id"			INTEGER NOT NULL,
	"composition_id"	INTEGER NOT NULL,
	"strain_id" 	INTEGER NOT NULL,
	"percentage"	NUMERIC NOT NULL CHECK("percentage" > 0.0 AND "percentage" <= 1.0),

	PRIMARY KEY ("id"),
	FOREIGN KEY ("strain_id") REFERENCES "strain"("id")
);

--- Holds records of different composition of recorded controls ---
CREATE TABLE IF NOT EXISTS "composition" (
	"id"			INTEGER NOT NULL,
    "index"         INTEGER, 
	"label"			VARCHAR NOT NULL,
	"legacy"		INTEGER NOT NULL,

	PRIMARY KEY("id"),
	CONSTRAINT "label_index_legacy_uc" UNIQUE("index","label","legacy")
);

CREATE TABLE IF NOT EXISTS "whole_blood_tube" (
	"id"			INTEGER NOT NULL,
	"barcode"		VARCHAR,
	"position"		VARCHAR NOT NULL,
	"cryovial_box_id"	INTEGER NOT NULL,

	PRIMARY KEY ("id"),
	FOREIGN KEY ("cryovial_box_id") REFERENCES "cryovial_box"("id")
);

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('2.0.0');