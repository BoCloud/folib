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
package com.folib.forms.license;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
/**
 * license vo
 * @author leipenghui
 */
public class LicenseTableForm implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    private Long id;

    /**
     * 创建时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 许可证id
     */
    private String licenseId;

    /**
     * 许可证名称
     */
    private String licenseName;

    /**
     * 许可证地址
     */
    private String licenseUrl;

    /**
     * 是否是自定义许可证 1是 0否
     */
    private Integer isCustomLicense;

    /**
     * 是否已弃用 1是 0否
     */
    private Integer isDeprecated;

    /**
     * 是否属于OSI-Approved授权协议 1是 0否
     */
    private Integer isOsiApproved;

    /**
     * 是否为自由软件基金会 1是 0否
     */
    private Integer isFsfLibre;

    /**
     * 黑白名单类型 1 白名单 2 黑名单
     */
    private Integer blackWhiteType;

    /**
     * 许可证头信息
     */
    private String header;

    /**
     * 许可证模板
     */
    private String template;

    /**
     * 许可证原文内容
     */
    private String content;

    /**
     * 许可证中文内容
     */
    private String contentCn;
    /**
     * 备注
     */
    private String comment;
}
