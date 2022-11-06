//
CREATE PROCEDURE save_property
(IN prop varchar(50), IN val varchar(500))
BEGIN
INSERT INTO properties (`id`, `value`) VALUES (prop, val)
    ON DUPLICATE KEY UPDATE `value`=val;
END//



//
CREATE PROCEDURE merge_ecosystem
(IN p_vendor VARCHAR(255), IN p_product VARCHAR(255), IN p_ecosystem varchar(255))
BEGIN
INSERT INTO cpeEcosystemCache (`vendor`, `product`, `ecosystem`) VALUES (p_vendor, p_product, p_ecosystem)
    ON DUPLICATE KEY UPDATE `ecosystem`=p_ecosystem;
END//


//
CREATE PROCEDURE cleanup_orphans()
BEGIN
SET @OLD_SQL_SAFE_UPDATES = (SELECT @@SQL_SAFE_UPDATES);
SET SQL_SAFE_UPDATES = 0;
DELETE FROM cpeEntry WHERE id not in (SELECT CPEEntryId FROM software);
SET SQL_SAFE_UPDATES = @OLD_SQL_SAFE_UPDATES;
END //



//
CREATE PROCEDURE update_vulnerability (
    IN p_cveId VARCHAR(20), IN p_description VARCHAR(8000), IN p_v2Severity VARCHAR(20),
    IN p_v2ExploitabilityScore DECIMAL(3,1), IN p_v2ImpactScore DECIMAL(3,1), IN p_v2AcInsufInfo BOOLEAN,
    IN p_v2ObtainAllPrivilege BOOLEAN, IN p_v2ObtainUserPrivilege BOOLEAN, IN p_v2ObtainOtherPrivilege BOOLEAN,
    IN p_v2UserInteractionRequired BOOLEAN, IN p_v2Score DECIMAL(3,1), IN p_v2AccessVector VARCHAR(20),
    IN p_v2AccessComplexity VARCHAR(20), IN p_v2Authentication VARCHAR(20), IN p_v2ConfidentialityImpact VARCHAR(20),
    IN p_v2IntegrityImpact VARCHAR(20), IN p_v2AvailabilityImpact VARCHAR(20), IN p_v2Version VARCHAR(5),
    IN p_v3ExploitabilityScore DECIMAL(3,1), IN p_v3ImpactScore DECIMAL(3,1), IN p_v3AttackVector VARCHAR(20),
    IN p_v3AttackComplexity VARCHAR(20), IN p_v3PrivilegesRequired VARCHAR(20), IN p_v3UserInteraction VARCHAR(20),
    IN p_v3Scope VARCHAR(20), IN p_v3ConfidentialityImpact VARCHAR(20), IN p_v3IntegrityImpact VARCHAR(20),
    IN p_v3AvailabilityImpact VARCHAR(20), IN p_v3BaseScore DECIMAL(3,1), IN p_v3BaseSeverity VARCHAR(20),
    IN p_v3Version VARCHAR(5))
BEGIN
DECLARE vulnerabilityId INT DEFAULT 0;

START TRANSACTION;

SET @OLD_SQL_SAFE_UPDATES = (SELECT @@SQL_SAFE_UPDATES);
SET @OLD_SQL_MODE = @@sql_mode;
SET SQL_SAFE_UPDATES = 0;
SET SQL_MODE = '';

SELECT id INTO vulnerabilityId FROM vulnerability WHERE cve=p_cveId;

IF vulnerabilityId > 0 THEN
DELETE FROM `reference` WHERE cveid = vulnerabilityId;
DELETE FROM software WHERE cveid = vulnerabilityId;
DELETE FROM cweEntry WHERE cveid = vulnerabilityId;
UPDATE vulnerability SET `description`=p_description,
                         `v2Severity`=p_v2Severity, `v2ExploitabilityScore`=p_v2ExploitabilityScore, `v2ImpactScore`=p_v2ImpactScore,
                         `v2AcInsufInfo`=p_v2AcInsufInfo, `v2ObtainAllPrivilege`=p_v2ObtainAllPrivilege,
                         `v2ObtainUserPrivilege`=p_v2ObtainUserPrivilege, `v2ObtainOtherPrivilege`=p_v2ObtainOtherPrivilege,
                         `v2UserInteractionRequired`=p_v2UserInteractionRequired, `v2Score`=p_v2Score, `v2AccessVector`=p_v2AccessVector,
                         `v2AccessComplexity`=p_v2AccessComplexity, `v2Authentication`=p_v2Authentication, `v2ConfidentialityImpact`=p_v2ConfidentialityImpact,
                         `v2IntegrityImpact`=p_v2IntegrityImpact, `v2AvailabilityImpact`=p_v2AvailabilityImpact, `v2Version`=p_v2Version,
                         `v3ExploitabilityScore`=p_v3ExploitabilityScore, `v3ImpactScore`=p_v3ImpactScore, `v3AttackVector`=p_v3AttackVector,
                         `v3AttackComplexity`=p_v3AttackComplexity, `v3PrivilegesRequired`=p_v3PrivilegesRequired, `v3UserInteraction`=p_v3UserInteraction,
                         `v3Scope`=p_v3Scope, `v3ConfidentialityImpact`=p_v3ConfidentialityImpact, `v3IntegrityImpact`=p_v3IntegrityImpact,
                         `v3AvailabilityImpact`=p_v3AvailabilityImpact, `v3BaseScore`=p_v3BaseScore, `v3BaseSeverity`=p_v3BaseSeverity, `v3Version`=p_v3Version
WHERE id=vulnerabilityId;
ELSE
    INSERT INTO vulnerability (`cve`, `description`,
        `v2Severity`, `v2ExploitabilityScore`,
        `v2ImpactScore`, `v2AcInsufInfo`, `v2ObtainAllPrivilege`,
        `v2ObtainUserPrivilege`, `v2ObtainOtherPrivilege`, `v2UserInteractionRequired`,
        `v2Score`, `v2AccessVector`, `v2AccessComplexity`,
        `v2Authentication`, `v2ConfidentialityImpact`, `v2IntegrityImpact`,
        `v2AvailabilityImpact`, `v2Version`, `v3ExploitabilityScore`,
        `v3ImpactScore`, `v3AttackVector`, `v3AttackComplexity`,
        `v3PrivilegesRequired`, `v3UserInteraction`, `v3Scope`,
        `v3ConfidentialityImpact`, `v3IntegrityImpact`, `v3AvailabilityImpact`,
        `v3BaseScore`, `v3BaseSeverity`, `v3Version`)
        VALUES (p_cveId, p_description,
        p_v2Severity, p_v2ExploitabilityScore,
        p_v2ImpactScore, p_v2AcInsufInfo, p_v2ObtainAllPrivilege,
        p_v2ObtainUserPrivilege, p_v2ObtainOtherPrivilege, p_v2UserInteractionRequired,
        p_v2Score, p_v2AccessVector, p_v2AccessComplexity,
        p_v2Authentication, p_v2ConfidentialityImpact, p_v2IntegrityImpact,
        p_v2AvailabilityImpact, p_v2Version, p_v3ExploitabilityScore,
        p_v3ImpactScore, p_v3AttackVector, p_v3AttackComplexity,
        p_v3PrivilegesRequired, p_v3UserInteraction, p_v3Scope,
        p_v3ConfidentialityImpact, p_v3IntegrityImpact, p_v3AvailabilityImpact,
        p_v3BaseScore, p_v3BaseSeverity, p_v3Version);

        SET vulnerabilityId = LAST_INSERT_ID();
END IF;
SET SQL_SAFE_UPDATES = @OLD_SQL_SAFE_UPDATES;
SET SQL_MODE = @OLD_SQL_MODE;

COMMIT;

SELECT vulnerabilityId;

END//

//
CREATE PROCEDURE insert_software (
    IN p_vulnerabilityId INT, IN p_part CHAR(1), IN p_vendor VARCHAR(255), IN p_product VARCHAR(255),
    IN p_version VARCHAR(255), IN p_update_version VARCHAR(255), IN p_edition VARCHAR(255), IN p_lang VARCHAR(20),
    IN p_sw_edition VARCHAR(255), IN p_target_sw VARCHAR(255), IN p_target_hw VARCHAR(255), IN p_other VARCHAR(255),
    IN p_ecosystem VARCHAR(255), IN p_versionEndExcluding VARCHAR(50), IN p_versionEndIncluding VARCHAR(50),
    IN p_versionStartExcluding VARCHAR(50), IN p_versionStartIncluding VARCHAR(50), IN p_vulnerable BOOLEAN)
BEGIN

    DECLARE cpeId INT DEFAULT 0;
    DECLARE currentEcosystem VARCHAR(255);

START TRANSACTION;

SET @OLD_SQL_SAFE_UPDATES = (SELECT @@SQL_SAFE_UPDATES);
    SET SQL_SAFE_UPDATES = 0;

SELECT id, ecosystem
INTO cpeId, currentEcosystem
FROM cpeEntry WHERE `part`=p_part AND `vendor`=p_vendor AND `product`=p_product
                AND `version`=p_version AND `update_version`=p_update_version AND `edition`=p_edition
                AND `lang`=p_lang AND `sw_edition`=p_sw_edition AND `target_sw`=p_target_sw
                AND `target_hw`=p_target_hw AND `other`=p_other
    LIMIT 1;

IF cpeId > 0 THEN
        IF currentEcosystem IS NULL AND p_ecosystem IS NOT NULL THEN
UPDATE cpeEntry SET `ecosystem`=p_ecosystem WHERE id=cpeId;
END IF;
ELSE
        INSERT INTO cpeEntry (`part`, `vendor`, `product`, `version`, `update_version`,
            `edition`, `lang`, `sw_edition`, `target_sw`, `target_hw`, `other`, `ecosystem`)
        VALUES (p_part, p_vendor, p_product, p_version, p_update_version,
                p_edition, p_lang, p_sw_edition, p_target_sw, p_target_hw, p_other, p_ecosystem);
        SET cpeId = LAST_INSERT_ID();
END IF;

INSERT INTO software (`cveid`, `cpeEntryId`, `versionEndExcluding`, `versionEndIncluding`,
                      `versionStartExcluding`, `versionStartIncluding`, `vulnerable`)
VALUES (p_vulnerabilityId, cpeId, p_versionEndExcluding, p_versionEndIncluding,
        p_versionStartExcluding, p_versionStartIncluding, p_vulnerable);

SET SQL_SAFE_UPDATES = @OLD_SQL_SAFE_UPDATES;
COMMIT;
END//

//
CREATE PROCEDURE update_ecosystems()
BEGIN
    SET @OLD_SQL_SAFE_UPDATES = (SELECT @@SQL_SAFE_UPDATES);
    SET SQL_SAFE_UPDATES = 0;
UPDATE cpeEntry e INNER JOIN cpeEcosystemCache c
ON c.vendor=e.vendor
    AND c.product=e.product
    SET e.ecosystem=c.ecosystem
WHERE e.ecosystem IS NULL AND c.ecosystem<>'MULTIPLE';

SET SQL_SAFE_UPDATES = @OLD_SQL_SAFE_UPDATES;
END//



//
CREATE PROCEDURE update_ecosystems2()
BEGIN
    SET @OLD_SQL_SAFE_UPDATES = (SELECT @@SQL_SAFE_UPDATES);
    SET SQL_SAFE_UPDATES = 0;
UPDATE cpeEntry e INNER JOIN cpeEcosystemCache c
ON c.vendor=e.vendor
    AND c.product=e.product
    SET e.ecosystem=null
WHERE c.ecosystem='MULTIPLE'
  AND e.ecosystem IS NOT NULL;

SET SQL_SAFE_UPDATES = @OLD_SQL_SAFE_UPDATES;
END//