ALTER TABLE "sample_type" ADD COLUMN "parent_id" INTEGER DEFAULT NULL REFERENCES "sample_type"("id");

--- Control Collection ---
CREATE TABLE IF NOT EXISTS "control_collection" (
	"id"			INTEGER NOT NULL,
	"url"			VARCHAR NOT NULL,
	"metadata"		TEXT,

	PRIMARY KEY("id")
);


ALTER TABLE "study" ADD COLUMN "control_collection_id" REFERENCES "control_collection"("id");

--- just remove dbs ---
DROP TABLE "dbs_spot";

--- insert new derived sample types

INSERT OR ROLLBACK INTO "sample_type" (name, parent_id)
VALUES
	("Tube", 3),
	("Paper", 3);

--- DBS Sheet ---
CREATE TABLE IF NOT EXISTS "dbs_control_sheet" (
	"id"			INTEGER NOT NULL,
	"bag_id"		INTEGER NOT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "dbs_bag"("id")
);


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

--- DBS Control ---
CREATE TABLE IF NOT EXISTS "dbs_control" (
	"id"			INTEGER NOT NULL,
	"dbs_control_sheet_id"	INTEGER NOT NULL,
	"position"		VARCHAR NOT NULL CHECK(length("position") == 3),

	PRIMARY KEY(id),
	FOREIGN KEY(id) REFERENCES "specimen"("id"),
	FOREIGN KEY(dbs_control_sheet_id) REFERENCES "dbs_control_sheet"("id")
);

--- Identifier for the control combination ---
CREATE TABLE IF NOT EXISTS "control" (
	"id"		INTEGER NOT NULL,
	"density"	REAL NOT NULL,

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "study_subject"("id")
);

CREATE TABLE IF NOT EXISTS "strain" (
	"id"			INTEGER NOT NULL,
	"name"			VARCHAR NOT NULL UNIQUE,
	"description"	VARCHAR,

	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "control_strain" (
	"id"			INTEGER NOT NULL,
	"control_id"	INTEGER NOT NULL,
	"strain_id" 	INTEGER NOT NULL,
	"percentage"	INTEGER NOT NULL CHECK("percentage" > 0 AND "percentage" <= 100),

	PRIMARY KEY ("id"),
	FOREIGN KEY ("control_id") REFERENCES "control"("id"),
	FOREIGN KEY ("strain_id") REFERENCES "strain"("id")
);

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('2.0.0');