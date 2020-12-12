BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "films" (
	"title"	TEXT,
	"year"	TEXT,
	"runtime"	TEXT,
	"actors"	TEXT,
	"averageratings"	REAL,
	PRIMARY KEY("title"),
	FOREIGN KEY("title") REFERENCES "films"
);
CREATE TABLE IF NOT EXISTS "actors" (
	"name"	TEXT NOT NULL,
	"title"	TEXT NOT NULL,
	FOREIGN KEY("title") REFERENCES "actors"("name"),
	FOREIGN KEY("name") REFERENCES "films"
);
COMMIT;
