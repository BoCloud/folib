//
CREATE PROCEDURE merge_knownexploited
(IN p_cveID varchar(20),
 IN p_vendorProject VARCHAR(255),
 IN p_product VARCHAR(255),
 IN p_vulnerabilityName VARCHAR(500),
 IN p_dateAdded CHAR(10),
 IN p_shortDescription VARCHAR(2000),
 IN p_requiredAction VARCHAR(1000),
 IN p_dueDate CHAR(10),
 IN p_notes VARCHAR(2000))
BEGIN
INSERT INTO knownExploited (`cveID`, `vendorProject`, `product`, `vulnerabilityName`,
            `dateAdded`, `shortDescription`, `requiredAction`, `dueDate`, `notes`)
       VALUES (p_cveID, p_vendorProject, p_product, p_vulnerabilityName, p_dateAdded,
            p_shortDescription, p_requiredAction, p_dueDate, p_notes)
       ON DUPLICATE KEY UPDATE `vendorProject`=p_vendorProject, `product`=p_product,
            `vulnerabilityName`=p_vulnerabilityName, `dateAdded`=p_dateAdded,
            `shortDescription`=p_shortDescription, `requiredAction`=p_requiredAction,
            `dueDate`=p_dueDate, `notes`=p_notes;
END//