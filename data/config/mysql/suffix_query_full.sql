) as q1
INNER JOIN objects obje ON (obje.guid = q1.guid)
INNER JOIN names n ON (n.id = obje.objectType)