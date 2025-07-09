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
package com.folib.storage.metadata;

import javax.annotation.Nonnull;
import java.util.regex.Matcher;

import static org.apache.maven.artifact.Artifact.VERSION_FILE_PATTERN;

/**
 * @author veadan
 */
public class SnapshotVersionDecomposition
{

    public static final SnapshotVersionDecomposition INVALID = new SnapshotVersionDecomposition(null,
                                                                                                Integer.MIN_VALUE,
                                                                                                null);

    private final String version;
    private final int buildNumber;
    private final String timestamp;

    @Nonnull
    public static SnapshotVersionDecomposition of(final String version)
    {
        if (version == null)
        {
            return INVALID;
        }

        final Matcher matcher = VERSION_FILE_PATTERN.matcher(version);
        if (!matcher.matches())
        {
            return INVALID;
        }

        final int buildNumber = Integer.parseInt(matcher.group(3));
        final String timestamp = matcher.group(2);

        return new SnapshotVersionDecomposition(version, buildNumber, timestamp);
    }

    private SnapshotVersionDecomposition(final String version,
                                         final int buildNumber,
                                         final String timestamp)
    {
        this.version = version;
        this.buildNumber = buildNumber;
        this.timestamp = timestamp;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        SnapshotVersionDecomposition that = (SnapshotVersionDecomposition) o;

        return version != null ? version.equals(that.version) : that.version == null;
    }

    @Override
    public int hashCode()
    {
        return version != null ? version.hashCode() : 0;
    }

    public String getVersion()
    {
        return version;
    }

    public int getBuildNumber()
    {
        return buildNumber;
    }

    public String getTimestamp()
    {
        return timestamp;
    }
}
