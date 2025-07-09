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

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;


/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "license")
@ApiModel("license")
public class License implements Serializable {
    private static final long serialVersionUID = 1L;


    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 许可证id
     */
    @ApiModelProperty("许可证id")
    @Column(name = "license_id")
    private String licenseId;

    /**
     * 许可证名称
     */
    @ApiModelProperty("许可证名称")
    @Column(name = "license_name")
    private String licenseName;

    /**
     * 许可证地址
     */
    @ApiModelProperty("许可证地址")
    @Column(name = "license_url")
    private String licenseUrl;

    /**
     * 是否是自定义许可证 1是 0否
     */
    @ApiModelProperty("是否是自定义许可证 1是 0否")
    @Column(name = "is_custom_license")
    private Integer isCustomLicense;

    /**
     * 是否已弃用 1是 0否
     */
    @ApiModelProperty("是否已弃用 1是 0否")
    @Column(name = "is_deprecated")
    private Integer isDeprecated;

    /**
     * 是否属于OSI-Approved授权协议 1是 0否
     */
    @ApiModelProperty("是否属于OSI-Approved授权协议 1是 0否")
    @Column(name = "is_osi_approved")
    private Integer isOsiApproved;

    /**
     * 是否为自由软件基金会 1是 0否
     */
    @ApiModelProperty("是否为自由软件基金会 1是 0否")
    @Column(name = "is_fsf_libre")
    private Integer isFsfLibre;

    /**
     * 黑白名单类型 1 白名单 2 黑名单
     */
    @ApiModelProperty("黑白名单类型 1 白名单 2 黑名单")
    @Column(name = "black_white_type")
    private Integer blackWhiteType;

    /**
     * 许可证头信息
     */
    @ApiModelProperty("许可证头信息")
    @Column(name = "header")
    private String header;

    /**
     * 许可证模板
     */
    @ApiModelProperty("许可证模板")
    @Column(name = "template")
    private String template;

    /**
     * 许可证原文内容
     */
    @ApiModelProperty("许可证原文内容")
    @Column(name = "content")
    private String content;

    /**
     * 许可证中文内容
     */
    @ApiModelProperty("许可证中文内容")
    @Column(name = "content_cn")
    private String contentCn;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Column(name = "comment")
    private String comment;
}
