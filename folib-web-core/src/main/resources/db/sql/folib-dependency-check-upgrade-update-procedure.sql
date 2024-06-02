//
CREATE OR REPLACE PROCEDURE merge_knownexploited(
    p_cveID IN knownExploited.cveID%type,
    p_vendorProject IN knownExploited.vendorProject%type,
    p_product IN knownExploited.product%type,
    p_vulnerabilityName IN knownExploited.vulnerabilityName%type,
    p_dateAdded IN knownExploited.dateAdded%type,
    p_shortDescription IN knownExploited.shortDescription%type,
    p_requiredAction IN knownExploited.requiredAction%type,
    p_dueDate IN knownExploited.dueDate%type,
    p_notes IN knownExploited.notes%type)
AS
BEGIN
    INSERT INTO knownExploited (cveID, vendorProject, product, vulnerabilityName,
            dateAdded, shortDescription, requiredAction, dueDate, notes)
    VALUES (p_cveID, p_vendorProject, p_product, p_vulnerabilityName, p_dateAdded,
            p_shortDescription, p_requiredAction, p_dueDate, p_notes);
EXCEPTION
    WHEN DUP_VAL_ON_INDEX THEN
        UPDATE knownExploited
        SET vendorProject=p_vendorProject, product=p_product, vulnerabilityName=p_vulnerabilityName,
            dateAdded=p_dateAdded, shortDescription=p_shortDescription, requiredAction=p_requiredAction,
            dueDate=p_dueDate, notes=p_notes
        WHERE cveID=p_cveID;
END;//