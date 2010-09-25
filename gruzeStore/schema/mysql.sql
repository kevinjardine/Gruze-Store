--
-- Table structure for table 'files'
--

CREATE TABLE files (
  id int(11) NOT NULL auto_increment,
  originalName text collate utf8_unicode_ci NOT NULL,
  contentType text collate utf8_unicode_ci NOT NULL,
  locationFile text collate utf8_unicode_ci NOT NULL,
  locationDir text collate utf8_unicode_ci NOT NULL,
  timeCreated int(11) NOT NULL default '0',
  PRIMARY KEY  (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ;

-- --------------------------------------------------------

--
-- Table structure for table 'metadata'
--

CREATE TABLE metadata (
  id int(11) NOT NULL auto_increment,
  objectGuid int(11) NOT NULL,
  metadataType int(11) NOT NULL,
  nameId int(11) NOT NULL,
  integerValue int(11) NOT NULL,
  stringValue text collate utf8_unicode_ci NOT NULL,
  PRIMARY KEY  (id),
  KEY object_guid (objectGuid,nameId)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ;

-- --------------------------------------------------------

--
-- Table structure for table 'names'
--

CREATE TABLE `names` (
  id int(11) NOT NULL auto_increment,
  `hash` int(11) NOT NULL,
  `string` text collate utf8_unicode_ci NOT NULL,
  PRIMARY KEY  (id),
  KEY `hash` (`hash`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ;

-- --------------------------------------------------------

--
-- Table structure for table 'objects'
--

CREATE TABLE objects (
  guid int(11) NOT NULL auto_increment,
  objectType int(11) NOT NULL,
  ownerGuid int(11) NOT NULL,
  containerGuid int(11) NOT NULL,
  siteGuid int(11) NOT NULL,
  timeCreated int(11) NOT NULL,
  timeUpdated int(11) NOT NULL,
  enabled tinyint(1) NOT NULL,
  PRIMARY KEY  (guid),
  KEY object_type (objectType,ownerGuid,containerGuid,siteGuid)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ;

-- --------------------------------------------------------

--
-- Table structure for table 'relationships'
--

CREATE TABLE relationships (
  id int(11) NOT NULL auto_increment,
  guid1 int(11) NOT NULL,
  guid2 int(11) NOT NULL,
  relationshipType int(11) NOT NULL,
  timeCreated int(11) NOT NULL,
  PRIMARY KEY  (id),
  KEY guid1 (guid1,guid2,relationshipType)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ;

-- --------------------------------------------------------

--
-- Table structure for table 'searchable'
--

CREATE TABLE searchable (
  typeID int(11) NOT NULL,
  nameID int(11) NOT NULL,
  UNIQUE KEY objectID (typeID,nameID)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
