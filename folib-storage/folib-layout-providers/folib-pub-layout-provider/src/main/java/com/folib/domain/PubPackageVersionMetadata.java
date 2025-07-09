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

import com.alibaba.fastjson.annotation.JSONField;
import com.github.zafarkhaja.semver.Version;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/6/13
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PubPackageVersionMetadata implements Serializable, Comparable<PubPackageVersionMetadata> {

    /**
     * 版本号。
     */
    private String version;

    /**
     * 是否被撤回。
     */
    private Boolean retracted;

    /**
     * 发布时间。
     */
    private String published;

    /**
     * 包配置信息
     */
    private Pubspec pubspec;

    /**
     * 制品包下载URL。
     * https://pub.dev/api/archives/meta-0.2.7.tar.gz
     * http://192.168.5.116:8081/artifactory/api/pub/pub-remote/packages/js/versions/0.0.26.tar.gz
     */
    @JSONField(name = "archive_url")
    private String archiveUrl;

    /**
     * 源制品包下载URL。
     */
    private String sourceArchiveUrl;

    /**
     * 制品包SHA256值。
     */
    @JSONField(name = "archive_sha256")
    private String archiveSha256;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        PubPackageVersionMetadata that = (PubPackageVersionMetadata) o;
        return version.equals(that.version);
    }

    @Override
    public int hashCode() {
        return Objects.hash(version);
    }

    @Override
    public int compareTo(@NotNull PubPackageVersionMetadata o) {
        return Version.valueOf(getVersion()).compareWithBuildsTo(Version.valueOf(o.getVersion()));
    }
}
