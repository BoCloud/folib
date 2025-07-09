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
package com.folib.indexer;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.constants.PubConstants;
import com.folib.domain.Pubspec;
import com.folib.providers.io.RepositoryPath;
import com.folib.utils.PubUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.io.IOUtils;
import org.javatuples.Pair;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.zip.GZIPInputStream;

/**
 * @author veadan
 * @date 2024/6/14
 **/
@Slf4j
public class PubMetadataExtractor {

    private final ObjectMapper yamlObjectMapper;

    public PubMetadataExtractor() {
        this.yamlObjectMapper = PubUtils.getPubYamlObjectMapper();
    }

    public Pubspec readFromPubSpecYaml(String entry) throws IOException {
        return (Pubspec) this.yamlObjectMapper.readValue(entry, Pubspec.class);
    }

    public Pair<Pubspec, Path> extractPubSpec(InputStream in)
            throws IOException {
        Path packageSourceTmp = Files.createTempFile("package", "source");
        Files.copy(in, packageSourceTmp, StandardCopyOption.REPLACE_EXISTING);
        Pubspec pubspec = null;
        try (InputStream tmpIn = new BufferedInputStream(Files.newInputStream(packageSourceTmp));
             GZIPInputStream gzipInputStream = new GZIPInputStream(tmpIn);
             TarArchiveInputStream tarInputStream = new TarArchiveInputStream(gzipInputStream)) {
            TarArchiveEntry entry;
            while ((entry = tarInputStream.getNextTarEntry()) != null) {
                // 如果是 pubspec.yaml 文件
                if (PubConstants.PUB_SPEC_YAML.equals(entry.getName())) {
                    String yamlString = IOUtils.toString(tarInputStream, StandardCharsets.UTF_8);
                    log.info("Current pubspec.yaml content [{}] ", yamlString);
                    // 解析 JSON 字符串
                    pubspec = readFromPubSpecYaml(yamlString);
                }
            }
        }
        if (pubspec == null || !Files.exists(packageSourceTmp)) {
            throw new IllegalArgumentException("Failed to parse pub package source pubspec not found");
        }
        return Pair.with(pubspec, packageSourceTmp);
    }

    public Pubspec extractPubSpec(RepositoryPath repositoryPath)
            throws IOException {
        Pubspec pubspec = null;
        try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(repositoryPath));
             GZIPInputStream gzipInputStream = new GZIPInputStream(inputStream);
             TarArchiveInputStream tarInputStream = new TarArchiveInputStream(gzipInputStream)) {
            TarArchiveEntry entry;
            while ((entry = tarInputStream.getNextTarEntry()) != null) {
                // 如果是 pubspec.yaml 文件
                if (PubConstants.PUB_SPEC_YAML.equals(entry.getName())) {
                    String yamlString = IOUtils.toString(tarInputStream, StandardCharsets.UTF_8);
                    log.info("RepositoryPath [{}] pubspec.yaml content [{}] ", repositoryPath, yamlString);
                    // 解析 JSON 字符串
                    pubspec = readFromPubSpecYaml(yamlString);
                }
            }
        }
        if (pubspec == null) {
            throw new IllegalArgumentException("Failed to parse pub package source pubspec not found");
        }
        return pubspec;
    }

}

