CREATE TABLE files (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  originalName TEXT,
  contentType TEXT,
  locationFile TEXT,
  locationDir TEXT,
  timeCreated INTEGER
);

CREATE TABLE metadata (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  objectGuid int(11) NOT NULL,
  metadataType int(11) NOT NULL,
  nameId int(11) NOT NULL,
  integerValue int(11) NOT NULL,
  stringValue text NOT NULL
);

CREATE TABLE `names` (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  `hash` int(11) NOT NULL,
  `string` text NOT NULL
);

CREATE TABLE objects (
  guid INTEGER PRIMARY KEY AUTOINCREMENT,
  objectType int(11) NOT NULL,
  ownerGuid int(11) NOT NULL,
  containerGuid int(11) NOT NULL,
  siteGuid int(11) NOT NULL,
  timeCreated int(11) NOT NULL,
  timeUpdated int(11) NOT NULL,
  enabled tinyint(1) NOT NULL
);

CREATE TABLE relationships (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  guid1 INTEGER,
  guid2 INTEGER,
  relationshipType INTEGER,
  timeCreated INTEGER
);

CREATE TABLE searchable (
  typeID int(11) NOT NULL,
  nameID int(11) NOT NULL
);
