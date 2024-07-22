--- Control composition definition table --- 
CREATE TABLE IF NOT EXISTS "NEW_composition_strain" (
	"id"			INTEGER NOT NULL,
	"composition_id"	INTEGER NOT NULL,
	"strain_id" 	INTEGER NOT NULL,
	"percentage"	NUMERIC NOT NULL CHECK("percentage" > 0.0 AND "percentage" <= 100.0),

	PRIMARY KEY ("id"),
	FOREIGN KEY ("strain_id") REFERENCES "strain"("id")
); --! COMMAND_END !--

INSERT OR ROLLBACK INTO "NEW_composition_strain" (id, composition_id, strain_id, percentage)
SELECT id, composition_id, strain_id, percentage * 100
FROM "composition_strain"; --! COMMAND_END !--

DROP TABLE IF EXISTS "composition_strain"; --! COMMAND_END !--
ALTER TABLE "NEW_composition_strain" RENAME TO "composition_strain"; --! COMMAND_END !--

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('2.0.2'); --! COMMAND_END !--
