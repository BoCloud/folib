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

import com.folib.util.annotations.PypiMetadataKey;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

public class PypiPackageInfo
{

    public static final String DISTRIBUTION_NAME_FORMAT = "^([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9._-]*[A-Za-z0-9])$";

//    public static final String VERSION_FORMAT = "^((\\d+!)?" +             // version epochs
//                                                "(\\d+(\\.\\d+)*)" +       // final releases
//                                                "((a|b|c|rc)\\d+)?" +      // pre releases
//                                                "(\\.post\\d+)?" +         // post releases
//                                                "(\\.dev\\d+)?" +          // developmental releases
//                                                "(\\+[.A-Z0-9]+)?)$";
    public static final String VERSION_FORMAT = "^(v)?(\\d+!)?" +
            "(\\d+(\\.\\d+)*)" +
            "([-_.]?([a-z]|[A-Z]|a|b|c|rc|alpha|beta|pre|preview)[-_.]?\\d*)?" +
            "((-(\\d+))|([-_.]?post[-_.]?\\d*)|([-_.]?r[-_.]?\\d*))?" +
            "([-_.]?dev[-_.]?\\d*)?" +
            "(\\+[a-z0-9]+([-_.][a-z0-9]+)*)?$";

    // local version identifiers
    public static final String NAME_FORMAT = "<a href=\"(.+?)\".*>(.+?\\.(:?exe|tar.gz|bz2|rpm|deb|zip|tgz|egg|dmg|msi|whl).*)</a>";

    @NotNull
    @PypiMetadataKey(name = "Metadata-Version")
    private PypiPackageInfo.SupportedMetadataVersionEnum metadataVersion;

    @NotBlank
    @Pattern(regexp = DISTRIBUTION_NAME_FORMAT, flags = Pattern.Flag.CASE_INSENSITIVE)
    @PypiMetadataKey(name = "Name")
    private String name;

    //version format validation done with PypiVersionValidator
    @NotBlank
    @PypiMetadataKey(name = "Version")
    private String version;

    @PypiMetadataKey(name = "Summary")
    private String summary;

    @PypiMetadataKey(name = "Home-page")
    private String homePage;

    @PypiMetadataKey(name = "Author")
    private String author;

    @PypiMetadataKey(name = "Author-email")
    private String authorEmail;

    @PypiMetadataKey(name = "License")
    private String license;

    @PypiMetadataKey(name = "Description-Content-Type")
    private String descriptionContentType;

    @PypiMetadataKey(name = "Description")
    private String description;

    @PypiMetadataKey(name = "Platform")
    private String platform;


    public PypiPackageInfo()
    {
    }

    public SupportedMetadataVersionEnum getMetadataVersion()
    {
        return metadataVersion;
    }

    public String getName()
    {
        return name;
    }

    public String getVersion()
    {
        return version;
    }

    public String getSummary()
    {
        return summary;
    }

    public String getHomePage()
    {
        return homePage;
    }

    public String getAuthor()
    {
        return author;
    }

    public String getAuthorEmail()
    {
        return authorEmail;
    }

    public String getLicense()
    {
        return license;
    }

    public String getDescriptionContentType()
    {
        return descriptionContentType;
    }

    public String getDescription()
    {
        return description;
    }

    public String getPlatform()
    {
        return platform;
    }

    public enum SupportedMetadataVersionEnum
    {
        VERSION_1_0("1.0"),
        VERSION_1_1("1.1"),
        VERSION_1_2("1.2"),
        VERSION_2_1("2.1");

        private String version;

        public String getVersionString()
        {
            return version;
        }

        public static SupportedMetadataVersionEnum getVersionEnum(String version)
                throws IllegalArgumentException
        {
            for (SupportedMetadataVersionEnum metadataVersionEnum : SupportedMetadataVersionEnum.values())
            {
                if (metadataVersionEnum.getVersionString().equals(version))
                {
                    return metadataVersionEnum;
                }
            }
            throw new IllegalArgumentException("Unsupported Metadata version: " + version);
        }

        SupportedMetadataVersionEnum(String version)
        {
            this.version = version;
        }
    }
}
