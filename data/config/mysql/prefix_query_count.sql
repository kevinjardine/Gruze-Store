SELECT count(DISTINCT q1.guid) as total
FROM (
    SELECT obj.guid FROM
    objects obj 