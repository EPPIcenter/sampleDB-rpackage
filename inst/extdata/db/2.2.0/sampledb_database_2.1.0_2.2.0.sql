CREATE TABLE IF NOT EXISTS "paper" (
	"id"	INTEGER NOT NULL,
	"bag_id"	INTEGER NOT NULL,
	"label"	VARCHAR NOT NULL,
	FOREIGN KEY("id") REFERENCES "storage_container"("id"),
	FOREIGN KEY("bag_id") REFERENCES "bag"("id"),
	PRIMARY KEY("id")
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

INSERT OR IGNORE INTO "version" ("name") VALUES ("2.2.0");
