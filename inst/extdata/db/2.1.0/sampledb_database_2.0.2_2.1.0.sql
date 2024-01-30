-- Drop the blood_spot_collection_dbs_control_sheet table
DROP TABLE IF EXISTS "blood_spot_collection_dbs_control_sheet";

-- Drop the blood_spot_collection table
DROP TABLE IF EXISTS "blood_spot_collection";

-- Recreate the blood_spot_collection table with the new dbs_control_sheet_id column
CREATE TABLE "blood_spot_collection" (
	"id"			INTEGER NOT NULL,
	"malaria_blood_control_id" INTEGER NOT NULL,
	"total"			INTEGER NOT NULL CHECK ("total" > 0),
	"exhausted"		INTEGER	NOT NULL DEFAULT 0 CHECK ("exhausted" <= "total" AND "exhausted" >= 0),
	"dbs_control_sheet_id"	INTEGER REFERENCES "dbs_control_sheet"("id"),
	PRIMARY KEY("id"),
	FOREIGN KEY("malaria_blood_control_id") REFERENCES "malaria_blood_control"("id")
);

-- Drop the control_collection table
DROP TABLE IF EXISTS "control_collection";

-- Update database version to reflect these changes
INSERT OR ROLLBACK INTO version (name) VALUES ('2.1.0');
