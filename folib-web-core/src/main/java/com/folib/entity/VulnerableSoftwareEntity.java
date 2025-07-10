/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.entity;

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
