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

-- Create a table to store archived whole blood
CREATE TABLE "archived_dbs_blood_spots" (
    "id" INTEGER NOT NULL,
    "blood_spot_collection_id" INTEGER NOT NULL,
    "archived_spots_count" INTEGER NOT NULL,
    "reason" TEXT,
    "archived_date" DATETIME NOT NULL DEFAULT current_timestamp,
    "status_id" INTEGER NOT NULL,
    PRIMARY KEY("id"),
    FOREIGN KEY("blood_spot_collection_id") REFERENCES "blood_spot_collection"("id"),
    FOREIGN KEY("status_id") REFERENCES "status"("id")
);

-- Add state, status and reason columns to whole blood
ALTER TABLE "whole_blood_tube" 
ADD COLUMN "state_id" INTEGER REFERENCES "state"("id");

ALTER TABLE "whole_blood_tube" 
ADD COLUMN "status_id" INTEGER REFERENCES "status"("id");

ALTER TABLE "whole_blood_tube" 
ADD COLUMN "reason" TEXT;

-- Current whole blood should default to 'Active' and 'In Use'
UPDATE "whole_blood_tube" 
SET "state_id" = 1, "status_id" = 1 
WHERE "state_id" IS NULL AND "status_id" IS NULL;


-- Update database version to reflect these changes
INSERT OR ROLLBACK INTO version (name) VALUES ('2.1.0');
