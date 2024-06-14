DROP TABLE IF EXISTS "dbs_tube";

DROP TABLE IF EXISTS "dbs_paper";

CREATE TABLE IF NOT EXISTS "paper" (
	"id"	INTEGER NOT NULL,
	"manifest_id"	INTEGER NOT NULL,
	"manifest_type" VARCHAR NOT NULL,
	"label"	VARCHAR NOT NULL,
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	PRIMARY KEY("id"),
	CHECK("manifest_type" IN ("box", "bag")),
	CONSTRAINT "label_container_uc" UNIQUE("label","manifest_id","manifest_type")
);

CREATE TABLE IF NOT EXISTS "box" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,
	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"	VARCHAR NOT NULL UNIQUE,
	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);

CREATE TABLE IF NOT EXISTS "bag" (
	"created"	DATETIME NOT NULL,
	"last_updated"	DATETIME NOT NULL,
	"id"	INTEGER NOT NULL,
	"location_id"	INTEGER NOT NULL,
	"name"	VARCHAR NOT NULL UNIQUE,
	FOREIGN KEY("location_id") REFERENCES "location"("id"),
	PRIMARY KEY("id")
);


INSERT OR IGNORE INTO "version" ("name") VALUES ("2.2.0");
