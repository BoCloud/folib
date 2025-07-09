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
package com.folib.storage.metadata.pypi;

/**
 * @author veadan
 */
public class PypiArtifactMetadata
{

    private String comment;

    private String metdataVersion;

    private String fileType;

    private String protcolVersion;

    private String author;

    private String homePage;

    private String downloadUrl;

    private String platform;

    private String version;

    private String description;

    private String md5Digest;

    private String action;

    private String name;

    private String license;

    private String pyVersion;

    private String summary;

    private String authorEmail;

    public String getComment()
    {
        return comment;
    }

    public PypiArtifactMetadata withComment(String comment)
    {
        this.comment = comment;
        return this;
    }

    public String getMetdataVersion()
    {
        return metdataVersion;
    }

    public PypiArtifactMetadata withMetdataVersion(String metdataVersion)
    {
        this.metdataVersion = metdataVersion;
        return this;
    }

    public String getFileType()
    {
        return fileType;
    }

    public PypiArtifactMetadata withFileType(String fileType)
    {
        this.fileType = fileType;
        return this;
    }

    public String getProtcolVersion()
    {
        return protcolVersion;
    }

    public PypiArtifactMetadata withProtcolVersion(String protcolVersion)
    {
        this.protcolVersion = protcolVersion;
        return this;
    }

    public String getAuthor()
    {
        return author;
    }

    public PypiArtifactMetadata withAuthor(String author)
    {
        this.author = author;
        return this;
    }

    public String getHomePage()
    {
        return homePage;
    }

    public PypiArtifactMetadata withHomePage(String homePage)
    {
        this.homePage = homePage;
        return this;
    }

    public String getDownloadUrl()
    {
        return downloadUrl;
    }

    public PypiArtifactMetadata withDownloadUrl(String downloadUrl)
    {
        this.downloadUrl = downloadUrl;
        return this;
    }

    public String getPlatform()
    {
        return platform;
    }

    public PypiArtifactMetadata withPlatform(String platform)
    {
        this.platform = platform;
        return this;
    }

    public String getVersion()
    {
        return version;
    }

    public PypiArtifactMetadata withVersion(String version)
    {
        this.version = version;
        return this;
    }

    public String getDescription()
    {
        return description;
    }

    public PypiArtifactMetadata withDescription(String description)
    {
        this.description = description;
        return this;
    }

    public String getMd5Digest()
    {
        return md5Digest;
    }

    public PypiArtifactMetadata withMd5Digest(String md5Digest)
    {
        this.md5Digest = md5Digest;
        return this;
    }

    public String getAction()
    {
        return action;
    }

    public PypiArtifactMetadata withAction(String action)
    {
        this.action = action;
        return this;
    }

    public String getName()
    {
        return name;
    }

    public PypiArtifactMetadata withName(String name)
    {
        this.name = name;
        return this;
    }

    public String getLicense()
    {
        return license;
    }

    public PypiArtifactMetadata withLicense(String license)
    {
        this.license = license;
        return this;
    }

    public String getPyVersion()
    {
        return pyVersion;
    }

    public PypiArtifactMetadata withPyVersion(String pyVersion)
    {
        this.pyVersion = pyVersion;
        return this;
    }

    public String getSummary()
    {
        return summary;
    }

    public PypiArtifactMetadata withSummary(String summary)
    {
        this.summary = summary;
        return this;
    }

    public String getAuthorEmail()
    {
        return authorEmail;
    }

    public PypiArtifactMetadata withAuthorEmail(String authorEmail)
    {
        this.authorEmail = authorEmail;
        return this;
    }

    @Override
    public String toString()
    {
        return "PypiArtifactMetadata [comment=" + comment + ", metdataVersion=" + metdataVersion + ", fileType="
                + fileType + ", protcolVersion=" + protcolVersion + ", author=" + author + ", homePage=" + homePage
                + ", downloadUrl=" + downloadUrl + ", platform=" + platform + ", version=" + version + ", description="
                + description + ", md5Digest=" + md5Digest + ", action=" + action + ", name=" + name + ", license="
                + license + ", pyVersion=" + pyVersion + ", summary=" + summary + ", authorEmail=" + authorEmail + "]";
    }

}
