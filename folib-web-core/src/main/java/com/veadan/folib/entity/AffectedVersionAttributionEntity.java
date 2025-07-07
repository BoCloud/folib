package com.veadan.folib.entity;


import lombok.Data;
import lombok.experimental.Accessors;

import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

@Data
@Accessors(chain = true)
@Table(name = "affected_version_attribution")
public class AffectedVersionAttributionEntity implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    private Long id;

    private Date firstSeen;

    private Date lastSeen;

    private String source;

    private String uuid;

    private String vulnerability;

    private String vulnerableSoftware;

}
