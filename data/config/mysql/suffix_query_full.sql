GROUP BY obj.guid
) as q1
INNER JOIN objects obj2 ON (obj2.guid = q1.guid)
INNER JOIN names n ON (n.id = obj2.objectType)