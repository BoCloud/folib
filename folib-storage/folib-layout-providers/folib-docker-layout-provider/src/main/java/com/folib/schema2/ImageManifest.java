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
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * The image manifest provides a configuration and a set of layers for a container image.
 * It’s the direct replacement for the schema-1 manifest.
 *
 * @author kalski
 *
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ImageManifest
{
    @JsonPropertyDescription("This field specifies the image manifest schema version as an integer. This schema uses version 2.")
    private Integer schemaVersion;

    @JsonPropertyDescription("The MIME type of the manifest. This should be set to application/vnd.docker.distribution.manifest.v2+json.")
    private String mediaType;

    @JsonPropertyDescription("Configuration object for a container.")
    private ContainerConfigurationManifest config;

    @JsonPropertyDescription("The layer list is ordered starting from the base image (opposite order of schema1).")
    private List<LayerManifest> layers;

    /**
     * manifests 多架构
     */
    private List<Manifests> manifests;

    private String digest;
}
