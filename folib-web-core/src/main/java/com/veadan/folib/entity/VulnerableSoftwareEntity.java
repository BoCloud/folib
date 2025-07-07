package com.veadan.folib.entity;

import lombok.Data;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.io.Serializable;

@Data
@Accessors(chain = true)
@Table( name ="vulnerable_software")
public class VulnerableSoftwareEntity implements Serializable {
    private static final long serialVersionUID = 1L;


    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    private Long id;

    private String cpe22;

    private String cpe23;

    private String edition;

    private String language;

    private String other;

    private String part;

    private String product;

    private String purl;

    private String purlName;

    private String purlNamespace;

    private String purlQualifiers;

    private String purlSubpath;

    private String purlType;

    private String purlVersion;

    private String swedition;

    private String targethw;

    private String targetsw;
     @Column(name = "`update`")
    private String update;

    private String uuid;

    private String vendor;

    private String version;

     @Column(name = "versionendexcluding")
    private String versionEndExcluding;

     @Column(name = "versionendincluding")
    private String versionEndIncluding;

     @Column(name = "versionstartexcluding")
    private String versionStartExcluding;

     @Column(name = "versionstartincluding")
    private String versionStartIncluding;

    private Boolean vulnerable;

}
