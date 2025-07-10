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
package com.folib.metadata.extractor;

import com.folib.metadata.model.Entry;
import com.folib.metadata.model.File;
import lombok.Data;
import org.redline_rpm.changelog.ChangelogEntry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.List;


@Data
public class RpmMetadata implements Serializable {

    private static final Logger log = LoggerFactory.getLogger(RpmMetadata.class);
    private String sha1Digest;
    private String artifactRelativePath;
    private long lastModified;
    private long size;
    private int headerStart;
    private int headerEnd;
    private String name;
    private String architecture;
    private String version;
    private int epoch;
    private String release;
    private String summary;
    private String description;
    private String packager;
    private String url;
    private int buildTime;
    private int installedSize;
    private int archiveSize;
    private String license;
    private String vendor;
    private String sourceRpm;
    private String buildHost;
    private String href;
    private String group;
    private List<Entry> provide;
    private List<Entry> require;
    private List<Entry> conflict;
    private List<Entry> obsolete;
    private List<Entry> recommends;
    private List<Entry> suggests;
    private List<File> files;
    private List<ChangelogEntry> changeLogs;
}
