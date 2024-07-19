-- Drop the blood_spot_collection_dbs_control_sheet table
DROP TABLE IF EXISTS "blood_spot_collection_dbs_control_sheet"; --! COMMAND_END !--

-- Drop the blood_spot_collection table
DROP TABLE IF EXISTS "blood_spot_collection"; --! COMMAND_END !--

-- Recreate the blood_spot_collection table with the new dbs_control_sheet_id and exhausted_at columns
CREATE TABLE "blood_spot_collection" (
    "id"                        INTEGER NOT NULL,
    "malaria_blood_control_id"  INTEGER NOT NULL,
    "total"                     INTEGER NOT NULL CHECK ("total" > 0),
    "exhausted"                 INTEGER NOT NULL DEFAULT 0 CHECK ("exhausted" <= "total" AND "exhausted" >= 0),
    "dbs_control_sheet_id"      INTEGER REFERENCES "dbs_control_sheet"("id"),
    PRIMARY KEY("id"),
    FOREIGN KEY("malaria_blood_control_id") REFERENCES "malaria_blood_control"("id")
); --! COMMAND_END !--

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
); --! COMMAND_END !--

-- Add state, status and reason columns to whole blood
ALTER TABLE "whole_blood_tube" 
ADD COLUMN "state_id" INTEGER REFERENCES "state"("id"); --! COMMAND_END !--

ALTER TABLE "whole_blood_tube" 
ADD COLUMN "status_id" INTEGER REFERENCES "status"("id"); --! COMMAND_END !--

ALTER TABLE "whole_blood_tube" 
ADD COLUMN "reason" TEXT; --! COMMAND_END !--

-- Current whole blood should default to 'Active' and 'In Use'
UPDATE "whole_blood_tube" 
SET "state_id" = 1, "status_id" = 1 
WHERE "state_id" IS NULL AND "status_id" IS NULL; --! COMMAND_END !--

-- Update database version to reflect these changes
INSERT OR ROLLBACK INTO "version" (name) VALUES ('2.1.0'); --! COMMAND_END !--
