SELECT sum(ma.integerValue) as total, count(DISTINCT obj.guid) as count 
FROM objects obj 