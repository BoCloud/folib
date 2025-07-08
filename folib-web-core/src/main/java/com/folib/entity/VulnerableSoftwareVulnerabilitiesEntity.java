package com.folib.entity;

import lombok.Data;

import javax.persistence.*;
import java.io.Serializable;

@Data
@Table(name = "vulnerable_software_vulnerabilities")
public class VulnerableSoftwareVulnerabilitiesEntity implements Serializable {
    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "vulnerability_Id")
    private String vulnerabilityId;

    @Column(name = "vulnerablesoftware_id")
    private String vulnerablesoftwareId;

}

