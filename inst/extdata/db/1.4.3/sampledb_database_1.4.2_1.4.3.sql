CREATE TABLE IF NOT EXISTS "NEW_cryovial_tube" (
	"id"			INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"barcode"		VARCHAR,
	"position"		VARCHAR CHECK(length("position") > 1 OR "position" IS NULL),

	PRIMARY KEY("id"),
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("manifest_id") REFERENCES "cryovial_box"("id"),

	CONSTRAINT "cryovial_tube_position_manifest_id_uc" UNIQUE("position", "manifest_id")
);

INSERT OR ROLLBACK INTO "NEW_cryovial_tube" (id, manifest_id, barcode, position)
SELECT id, manifest_id, barcode, position
FROM "cryovial_tube";

DROP TABLE IF EXISTS "cryovial_tube";
ALTER TABLE "NEW_cryovial_tube" RENAME TO "cryovial_tube";

--- update database version ---
INSERT OR ROLLBACK INTO version (name) VALUES ('1.4.3');
