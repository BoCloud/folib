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
package com.folib.util;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import com.folib.model.HelmIndexYamlMetadata;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public abstract class HelmUtils {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(HelmUtils.class);

    private final int maxSizeLimit=536870912;
    private static ObjectMapper mapper;

    public static boolean isHelmChartFile(String fileName) {
        return fileName.endsWith(".tgz");
    }

    public static boolean isMetadataFile(String fileName) {
        return fileName.endsWith("index.yaml");
    }

    public static ObjectMapper getYamlObjectMapper() {
        if (mapper == null) {
            mapper = new ObjectMapper((JsonFactory)createYamlFactory()) {
                public byte[] writeValueAsBytes(Object value) throws JsonProcessingException {
                    return writeValueAsString(value).getBytes(StandardCharsets.UTF_8);
                }

                public String writeValueAsString(Object value) throws JsonProcessingException {
                    String output = super.writeValueAsString(value);
                    return HelmVersionUtil.addQuotesToVersionsAttributes(output);
                }
            };
            mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        }
        return mapper;
    }

    public static HelmIndexYamlMetadata emptyIndexYaml() {
        HelmIndexYamlMetadata indexYaml = new HelmIndexYamlMetadata();
        indexYaml.apiVersion = "v1";
        indexYaml.generated = Instant.now().toString();
        return indexYaml;
    }


    //public static String getChartsBaseUrl(String remoteRepoKey, PackageHandlerArtifactoryConfigService packageHandlerRepoDescriptorService) {
    //    String res = packageHandlerRepoDescriptorService.getStringValue(remoteRepoKey, PackageDescriptorKey.HELM_CHARTS_BASE_URL, null);
    //    if (StringUtils.isBlank(res)) {
    //        res = packageHandlerRepoDescriptorService.getStringValue(remoteRepoKey, PackageDescriptorKey.URL, "");
    //    }
    //    return PathUtils.addTrailingSlash(res);
    //}

    //@Nullable
    //public static String getBaseUrlWithOverrideContextPathAndRepoKey(@Nullable String repoKey, @Nullable HttpServletRequest httpServletRequest, PackageHandlerArtifactoryConfigService packageHandlerRepoDescriptorService) {
    //    String suffix = (repoKey != null) ? ("/" + repoKey) : "";
    //    String res = packageHandlerRepoDescriptorService.getArtifactoryBaseUrl(httpServletRequest);
    //    if (StringUtils.isNotBlank(res)) {
    //        return PathUtils.trimTrailingSlashes(res) + PathUtils.trimTrailingSlashes(res);
    //    }
    //    return null;
    //}

    //public static String getHelmContextUrlFromPackageRequest(PackageRequestContext packageRequestContext) {
    //    String res = (String)packageRequestContext.getRequestHeaders().get("X-Orig-Client-Uri");
    //    if (StringUtils.isBlank(res)) {
    //        res = packageRequestContext.getServletContextUrl();
    //    }
    //    return res;
    //}

    public static InputStream indexYamlToInputStream(HelmIndexYamlMetadata indexYaml) {
        try {
            indexYaml.generated = Instant.now().toString();
           return new ByteArrayInputStream( writeIndexYaml(indexYaml).getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
            log.error("Failed to write index.yaml:{}", e.getMessage());
            log.debug("Failed to write index.yaml:", e);
            throw new RuntimeException(e.getMessage(), e);
        }
    }



    public static String writeIndexYaml(HelmIndexYamlMetadata helmIndexYamlMetadata) throws JsonProcessingException {
        return getYamlObjectMapper().writeValueAsString(helmIndexYamlMetadata);
    }

    private static YAMLFactory createYamlFactory() {
        //LoaderOptions loaderOptions = new LoaderOptions();
        //loaderOptions.setCodePointLimit(536870912);
        return YAMLFactory.builder().configure(YAMLGenerator.Feature.MINIMIZE_QUOTES, true)
                .configure(YAMLGenerator.Feature.ALWAYS_QUOTE_NUMBERS_AS_STRINGS, true)
                .configure(YAMLGenerator.Feature.WRITE_DOC_START_MARKER, false)
                .configure(YAMLGenerator.Feature.WRITE_DOC_START_MARKER, false)
                .configure(YAMLGenerator.Feature.SPLIT_LINES, false)
                //.loaderOptions(loaderOptions)
                .build();
    }

}

