SELECT q1.guid, n.string, timeCreated, timeUpdated, ownerGuid, containerGuid, siteGuid, enabled 
FROM (
    SELECT obj.guid FROM
    objects obj 