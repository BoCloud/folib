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
package com.folib.domain;

import com.folib.data.domain.DomainObject;

import java.time.LocalDateTime;
import java.util.Set;

/**
 * 组件
 *
 * @author veadan
 **/
public interface Component extends DomainObject {

    /**
     * 设置id
     *
     * @param id id
     */
    void setNativeId(Long id);

    /**
     * 获取创建时间
     *
     * @return 创建时间
     */
    LocalDateTime getCreated();

    /**
     * 设置创建时间
     *
     * @param created 创建时间
     */
    void setCreated(LocalDateTime created);

    /**
     * 获取最后更新时间
     *
     * @return 最后更新时间
     */
    LocalDateTime getLastUpdated();

    /**
     * 设置最后更新时间
     *
     * @param lastUpdated 最后更新时间
     */
    void setLastUpdated(LocalDateTime lastUpdated);

    /**
     * 获取组件名称
     *
     * @return 组件名称
     */
    String getName();

    /**
     * 设置组件名称
     *
     * @param name 组件名称
     */
    void setName(String name);

    /**
     * 获取文件名
     *
     * @return 文件名
     */
    String getFileName();

    /**
     * 设置文件名
     *
     * @param fileName 文件名
     */
    void setFileName(String fileName);

    /**
     * 获取版本
     *
     * @return 版本
     */
    String getVersion();

    /**
     * 设置版本
     *
     * @param version 版本
     */
    void setVersion(String version);

    /**
     * 获取组
     *
     * @return 组
     */
    String getGroupId();

    /**
     * 设置组
     *
     * @param groupId 组
     */
    void setGroupId(String groupId);

    /**
     * 获取描述
     *
     * @return 描述
     */
    String getDescription();

    /**
     * 设置描述
     *
     * @param description 描述
     */
    void setDescription(String description);

    /**
     * 获取pkg地址
     *
     * @return pkg地址
     */
    String getPurl();

    /**
     * 设置pkg地址
     *
     * @param purl pkg地址
     */
    void setPurl(String purl);

    /**
     * 获取url
     *
     * @return url
     */
    String getUrl();

    /**
     * 设置url
     *
     * @param url url
     */
    void setUrl(String url);

    /**
     * 获取通用平台枚举
     *
     * @return 通用平台枚举
     */
    String getCpe();

    /**
     * 设置通用平台枚举
     *
     * @param cpe 通用平台枚举
     */
    void setCpe(String cpe);

    /**
     * 获取md5sum
     *
     * @return md5sum
     */
    String getMd5sum();

    /**
     * 设置md5sum
     *
     * @param md5sum md5sum
     */
    void setMd5sum(String md5sum);

    /**
     * 获取sha1sum
     *
     * @return 通用sha1sum
     */
    String getSha1sum();

    /**
     * 设置sha1sum
     *
     * @param sha1sum sha1sum
     */
    void setSha1sum(String sha1sum);

    /**
     * 获取sha256sum
     *
     * @return sha256sum
     */
    String getSha256sum();

    /**
     * 设置sha256sum
     *
     * @param sha256sum sha256sum
     */
    void setSha256sum(String sha256sum);

    /**
     * 获取license
     *
     * @return license
     */
    Set<String> getLicense();

    /**
     * 设置license
     *
     * @param license license
     */
    void setLicense(Set<String> license);

    /**
     * 获取漏洞
     *
     * @return 漏洞
     */
    Set<String> getVulnerabilities();

    /**
     * 设置漏洞
     *
     * @param vulnerabilities 漏洞
     */
    void setVulnerabilities(Set<String> vulnerabilities);

    /**
     * 获取漏洞总数
     *
     * @return 漏洞总数
     */
    Integer getVulnerabilitiesCount();

    /**
     * 设置漏洞总数
     *
     * @param vulnerabilitiesCount 漏洞总数
     */
    void setVulnerabilitiesCount(Integer vulnerabilitiesCount);

    /**
     * 获取严重的漏洞数量
     *
     * @return 严重的漏洞数量
     */
    Integer getCriticalVulnerabilitiesCount();

    /**
     * 设置严重的漏洞数量
     *
     * @param criticalVulnerabilitiesCount 严重的漏洞数量
     */
    void setCriticalVulnerabilitiesCount(Integer criticalVulnerabilitiesCount);

    /**
     * 获取高危的漏洞数量
     *
     * @return 高危的漏洞数量
     */
    Integer getHighVulnerabilitiesCount();

    /**
     * 设置高危的漏洞数量
     *
     * @param highVulnerabilitiesCount 高危的漏洞数量
     */
    void setHighVulnerabilitiesCount(Integer highVulnerabilitiesCount);

    /**
     * 获取中危的漏洞数量
     *
     * @return 中危的漏洞数量
     */
    Integer getMediumVulnerabilitiesCount();

    /**
     * 设置中危的漏洞数量
     *
     * @param mediumVulnerabilitiesCount 中危的漏洞数量
     */
    void setMediumVulnerabilitiesCount(Integer mediumVulnerabilitiesCount);

    /**
     * 获取低危的漏洞数量
     *
     * @return 低危的漏洞数量
     */
    Integer getLowVulnerabilitiesCount();

    /**
     * 设置低危的漏洞数量
     *
     * @param lowVulnerabilitiesCount 低危的漏洞数量
     */
    void setLowVulnerabilitiesCount(Integer lowVulnerabilitiesCount);

    /**
     * 获取被封存的漏洞数量
     *
     * @return 被封存的漏洞数量
     */
    Integer getSuppressedVulnerabilitiesCount();

    /**
     * 设置被封存的漏洞数量
     *
     * @param suppressedVulnerabilitiesCount 被封存的漏洞数量
     */
    void setSuppressedVulnerabilitiesCount(Integer suppressedVulnerabilitiesCount);
}
