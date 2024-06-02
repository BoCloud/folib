DROP PROCEDURE IF EXISTS merge_knownexploited;
DROP TABLE IF EXISTS knownExploited;

UPDATE properties SET "value" = '5.4' WHERE id = 'version';

UPDATE cpeEcosystemCache set ecosystem='MULTIPLE' where vendor = 'icu-project' and product = 'international_components_for_unicode';
INSERT INTO cpeEcosystemCache (vendor, product, ecosystem) VALUES ('unicode', 'international_components_for_unicode', 'MULTIPLE');

ALTER TABLE software MODIFY versionEndExcluding VARCHAR(100);
ALTER TABLE software MODIFY versionEndIncluding VARCHAR(100);
ALTER TABLE software MODIFY versionStartExcluding VARCHAR(100);
ALTER TABLE software MODIFY versionStartIncluding VARCHAR(100);

CREATE TABLE knownExploited (cveID varchar(20) PRIMARY KEY ,
    vendorProject VARCHAR(255),
    product VARCHAR(255),
    vulnerabilityName VARCHAR(500),
    dateAdded CHAR(10),
    shortDescription VARCHAR(2000),
    requiredAction VARCHAR(1000),
    dueDate CHAR(10),
    notes VARCHAR(2000));