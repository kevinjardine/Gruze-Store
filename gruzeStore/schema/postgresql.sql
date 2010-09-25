CREATE TABLE files (
  id SERIAL,
  originalName TEXT NOT NULL,
  contentType TEXT NOT NULL,
  locationFile TEXT NOT NULL,
  locationDir TEXT NOT NULL,
  timeCreated INTEGER NOT NULL
);

CREATE TABLE metadata (
  id SERIAL,
  objectGuid INTEGER NOT NULL,
  metadataType INTEGER NOT NULL,
  nameId INTEGER NOT NULL,
  integerValue INTEGER NOT NULL,
  stringValue text NOT NULL
);

CREATE TABLE names (
  id SERIAL,
  hash INTEGER NOT NULL,
  string text NOT NULL
);

CREATE TABLE objects (
  guid SERIAL,
  objectType INTEGER NOT NULL,
  ownerGuid INTEGER NOT NULL,
  containerGuid INTEGER NOT NULL,
  siteGuid INTEGER NOT NULL,
  timeCreated INTEGER NOT NULL,
  timeUpdated INTEGER NOT NULL,
  enabled INTEGER NOT NULL
);

CREATE TABLE relationships (
  id SERIAL,
  guid1 INTEGER,
  guid2 INTEGER,
  relationshipType INTEGER,
  timeCreated INTEGER
);

CREATE TABLE searchable (
  typeID INTEGER NOT NULL,
  nameID INTEGER NOT NULL
);
