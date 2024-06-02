//
CREATE OR REPLACE PROCEDURE save_property(prop IN properties.id%type, val IN properties.value%type) AS
BEGIN
    INSERT INTO properties (id, value) VALUES (prop, val);
EXCEPTION
    WHEN DUP_VAL_ON_INDEX THEN
        UPDATE properties SET value=val WHERE id=prop;
END;//



//
CREATE OR REPLACE PROCEDURE merge_ecosystem(p_vendor IN cpeEcosystemCache.vendor%type, p_product IN cpeEcosystemCache.product%type, p_ecosystem IN cpeEcosystemCache.ecosystem%type) AS
BEGIN
    INSERT INTO cpeEcosystemCache (vendor, product, ecosystem) VALUES (p_vendor, p_product, p_ecosystem);
EXCEPTION
    WHEN DUP_VAL_ON_INDEX THEN
        UPDATE cpeEcosystemCache SET ecosystem=p_ecosystem WHERE product=p_product AND vendor=p_vendor;
END;//


//
CREATE OR REPLACE PROCEDURE cleanup_orphans() AS
BEGIN
    DELETE FROM cpeEntry WHERE id NOT IN (SELECT CPEEntryId FROM software);
END;
//



//
CREATE OR REPLACE PROCEDURE update_vulnerability(p_cveId IN vulnerability.cve%type,
                                      p_description IN vulnerability.description%type,
                                      p_v2Severity IN vulnerability.v2Severity%type,
                                      p_v2ExploitabilityScore IN vulnerability.v2ExploitabilityScore%type,
                                      p_v2ImpactScore IN vulnerability.v2ImpactScore%type,
                                      p_v2AcInsufInfo IN vulnerability.v2AcInsufInfo%type,
                                      p_v2ObtainAllPrivilege IN vulnerability.v2ObtainAllPrivilege%type,
                                      p_v2ObtainUserPrivilege IN vulnerability.v2ObtainUserPrivilege%type,
                                      p_v2ObtainOtherPrivilege IN vulnerability.v2ObtainOtherPrivilege%type,
                                      p_v2UserInteractionRequired IN vulnerability.v2UserInteractionRequired%type,
                                      p_v2Score IN vulnerability.v2Score%type,
                                      p_v2AccessVector IN vulnerability.v2AccessVector%type,
                                      p_v2AccessComplexity IN vulnerability.v2AccessComplexity%type,
                                      p_v2Authentication IN vulnerability.v2Authentication%type,
                                      p_v2ConfidentialityImpact IN vulnerability.v2ConfidentialityImpact%type,
                                      p_v2IntegrityImpact IN vulnerability.v2IntegrityImpact%type,
                                      p_v2AvailabilityImpact IN vulnerability.v2AvailabilityImpact%type,
                                      p_v2Version IN vulnerability.v2Version%type,
                                      p_v3ExploitabilityScore IN vulnerability.v3ExploitabilityScore%type,
                                      p_v3ImpactScore IN vulnerability.v3ImpactScore%type,
                                      p_v3AttackVector IN vulnerability.v3AttackVector%type,
                                      p_v3AttackComplexity IN vulnerability.v3AttackComplexity%type,
                                      p_v3PrivilegesRequired IN vulnerability.v3PrivilegesRequired%type,
                                      p_v3UserInteraction IN vulnerability.v3UserInteraction%type,
                                      p_v3Scope IN vulnerability.v3Scope%type,
                                      p_v3ConfidentialityImpact IN vulnerability.v3ConfidentialityImpact%type,
                                      p_v3IntegrityImpact IN vulnerability.v3IntegrityImpact%type,
                                      p_v3AvailabilityImpact IN vulnerability.v3AvailabilityImpact%type,
                                      p_v3BaseScore IN vulnerability.v3BaseScore%type,
                                      p_v3BaseSeverity IN vulnerability.v3BaseSeverity%type,
                                      p_v3Version IN vulnerability.v3Version%type,
                                      vulnerabilityId OUT vulnerability.id%type)
                                       AS
BEGIN
    BEGIN
        SELECT id into vulnerabilityId FROM vulnerability WHERE cve = p_cveId;
        DELETE FROM reference WHERE cveid = vulnerabilityId;
        DELETE FROM software WHERE cveid = vulnerabilityId;
        DELETE FROM cweEntry WHERE cveid = vulnerabilityId;
        UPDATE vulnerability
        SET description=p_description,
            v2Severity=p_v2Severity,
            v2ExploitabilityScore=p_v2ExploitabilityScore,
            v2ImpactScore=p_v2ImpactScore,
            v2AcInsufInfo=p_v2AcInsufInfo,
            v2ObtainAllPrivilege=p_v2ObtainAllPrivilege,
            v2ObtainUserPrivilege=p_v2ObtainUserPrivilege,
            v2ObtainOtherPrivilege=p_v2ObtainOtherPrivilege,
            v2UserInteractionRequired=p_v2UserInteractionRequired,
            v2Score=p_v2Score,
            v2AccessVector=p_v2AccessVector,
            v2AccessComplexity=p_v2AccessComplexity,
            v2Authentication=p_v2Authentication,
            v2ConfidentialityImpact=p_v2ConfidentialityImpact,
            v2IntegrityImpact=p_v2IntegrityImpact,
            v2AvailabilityImpact=p_v2AvailabilityImpact,
            v2Version=p_v2Version,
            v3ExploitabilityScore=p_v3ExploitabilityScore,
            v3ImpactScore=p_v3ImpactScore,
            v3AttackVector=p_v3AttackVector,
            v3AttackComplexity=p_v3AttackComplexity,
            v3PrivilegesRequired=p_v3PrivilegesRequired,
            v3UserInteraction=p_v3UserInteraction,
            v3Scope=p_v3Scope,
            v3ConfidentialityImpact=p_v3ConfidentialityImpact,
            v3IntegrityImpact=p_v3IntegrityImpact,
            v3AvailabilityImpact=p_v3AvailabilityImpact,
            v3BaseScore=p_v3BaseScore,
            v3BaseSeverity=p_v3BaseSeverity,
            v3Version=p_v3Version
        WHERE id = vulnerabilityId;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            INSERT INTO vulnerability (cve, description,
                                       v2Severity, v2ExploitabilityScore,
                                       v2ImpactScore, v2AcInsufInfo, v2ObtainAllPrivilege,
                                       v2ObtainUserPrivilege, v2ObtainOtherPrivilege, v2UserInteractionRequired,
                                       v2Score, v2AccessVector, v2AccessComplexity,
                                       v2Authentication, v2ConfidentialityImpact, v2IntegrityImpact,
                                       v2AvailabilityImpact, v2Version, v3ExploitabilityScore,
                                       v3ImpactScore, v3AttackVector, v3AttackComplexity,
                                       v3PrivilegesRequired, v3UserInteraction, v3Scope,
                                       v3ConfidentialityImpact, v3IntegrityImpact, v3AvailabilityImpact,
                                       v3BaseScore, v3BaseSeverity, v3Version)
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
                    p_v3BaseScore, p_v3BaseSeverity, p_v3Version)
            RETURNING id INTO vulnerabilityId;
        WHEN OTHERS THEN
            RAISE;
    END;
END;//

//
CREATE OR REPLACE PROCEDURE insert_software(p_vulnerabilityId IN software.cveid%type,
                                 p_part IN cpeEntry.part%type,
                                 p_vendor IN cpeEntry.vendor%type,
                                 p_product IN cpeEntry.product%type,
                                 p_version IN cpeEntry.version%type,
                                 p_update_version IN cpeEntry.update_version%type,
                                 p_edition IN cpeEntry.edition%type,
                                 p_lang IN cpeEntry.lang%type,
                                 p_sw_edition IN cpeEntry.sw_edition%type,
                                 p_target_sw IN cpeEntry.target_sw%type,
                                 p_target_hw IN cpeEntry.target_hw%type,
                                 p_other IN cpeEntry.other%type,
                                 p_ecosystem IN cpeEntry.ecosystem%type,
                                 p_versionEndExcluding IN software.versionEndExcluding%type,
                                 p_versionEndIncluding IN software.versionEndIncluding%type,
                                 p_versionStartExcluding IN software.versionStartExcluding%type,
                                 p_versionStartIncluding IN software.versionStartIncluding%type,
                                 p_vulnerable IN software.vulnerable%type) AS
    cpeId cpeEntry.id%type;
    currentEcosystem cpeEntry.ecosystem%type;
BEGIN
    BEGIN
        SELECT id, ecosystem
        INTO cpeId, currentEcosystem
        FROM cpeEntry
        WHERE part = p_part
          AND vendor = p_vendor
          AND product = p_product
          AND version = p_version
          AND update_version = p_update_version
          AND edition = p_edition
          AND lang = p_lang
          AND sw_edition = p_sw_edition
          AND target_sw = p_target_sw
          AND target_hw = p_target_hw
          AND other = p_other;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
           cpeId := 0;
           currentEcosystem := NULL;
        WHEN OTHERS THEN
           RAISE;
    END;

    IF cpeId > 0 THEN
        IF currentEcosystem IS NULL AND p_ecosystem IS NOT NULL THEN
            UPDATE cpeEntry SET ecosystem=p_ecosystem WHERE id = cpeId;
        END IF;
    ELSE
        INSERT INTO cpeEntry (part,
                              vendor,
                              product,
                              version,
                              update_version,
                              edition,
                              lang,
                              sw_edition,
                              target_sw,
                              target_hw,
                              other,
                              ecosystem)
        VALUES (p_part,
                p_vendor,
                p_product,
                p_version,
                p_update_version,
                p_edition,
                p_lang,
                p_sw_edition,
                p_target_sw,
                p_target_hw,
                p_other,
                p_ecosystem)
        RETURNING id INTO cpeId;
    END IF;

    INSERT INTO software (cveid,
                          cpeEntryId,
                          versionEndExcluding,
                          versionEndIncluding,
                          versionStartExcluding,
                          versionStartIncluding,
                          vulnerable)
    VALUES (p_vulnerabilityId,
            cpeId,
            p_versionEndExcluding,
            p_versionEndIncluding,
            p_versionStartExcluding,
            p_versionStartIncluding,
            p_vulnerable);

END;//

//
CREATE OR REPLACE PROCEDURE update_ecosystems() AS
BEGIN
    UPDATE cpeEntry e
    INNER JOIN cpeEcosystemCache c
    ON c.vendor = e.vendor AND c.product = e.product
    SET e.ecosystem = c.ecosystem
    WHERE e.ecosystem IS NULL AND c.ecosystem <> 'MULTIPLE';
END;
//

//
CREATE OR REPLACE PROCEDURE update_ecosystems2() AS
BEGIN
    UPDATE cpeEntry e
    INNER JOIN cpeEcosystemCache c
    ON c.vendor = e.vendor AND c.product = e.product
    SET e.ecosystem = NULL
    WHERE c.ecosystem = 'MULTIPLE' AND e.ecosystem IS NOT NULL;
END;
//

//
CREATE OR REPLACE VIEW v_update_ecosystems AS
    SELECT e.ecosystem AS entryEco, c.ecosystem AS cachedEco
    FROM cpeEntry e INNER JOIN cpeEcosystemCache c
    ON c.vendor=e.vendor
        AND c.product=e.product;
//