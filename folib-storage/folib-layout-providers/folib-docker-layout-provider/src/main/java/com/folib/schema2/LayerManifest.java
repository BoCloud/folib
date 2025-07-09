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
package com.folib.schema2;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;

import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class LayerManifest
{
    @JsonPropertyDescription("The MIME type of the referenced object. This should generally be application/vnd.docker.image.rootfs.diff.tar.gzip. "
            + "Layers of type application/vnd.docker.image.rootfs.foreign.diff.tar.gzip may be pulled from a remote location but they should never be pushed.")
    private String mediaType;
    
    @JsonPropertyDescription("The size in bytes of the object. This field exists "
            + "so that a client will have an expected size for the content before validating. If the length of the retrieved content does not match the specified length, "
            + "the content should not be trusted.")
    private Long size;
    
    @JsonPropertyDescription("The digest of the content, as defined by the Registry V2 HTTP API Specificiation.")
    private String digest;
    
    @JsonPropertyDescription("Provides a list of URLs from which the content may be fetched. Content should be verified against the digest and size. "
            + "This field is optional and uncommon.")
    private List<String> urls;

    public String getMediaType()
    {
        return mediaType;
    }

    public void setMediaType(String mediaType)
    {
        this.mediaType = mediaType;
    }

    public Long getSize()
    {
        return size;
    }

    public void setSize(Long size)
    {
        this.size = size;
    }

    public String getDigest()
    {
        return digest;
    }

    public void setDigest(String digest)
    {
        this.digest = digest;
    }

    public List<String> getUrls()
    {
        return urls;
    }

    public void setUrls(List<String> urls)
    {
        this.urls = urls;
    }
}
