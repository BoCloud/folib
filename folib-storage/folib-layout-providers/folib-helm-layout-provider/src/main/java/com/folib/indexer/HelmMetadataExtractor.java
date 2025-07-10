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
import com.fasterxml.jackson.databind.exc.MismatchedInputException;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;

import com.folib.model.HelmChartMetadata;
import com.folib.model.HelmDependenciesMetadata;
import com.folib.model.HelmMetadata;
import com.folib.model.HelmMetadataBuilder;
import com.folib.providers.io.RepositoryPath;
import com.folib.util.HelmUtils;
import lombok.Generated;
import lombok.NonNull;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HelmMetadataExtractor {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(HelmMetadataExtractor.class);

    private final ObjectMapper yamlObjectMapper;

    public HelmMetadataExtractor() {
        this.yamlObjectMapper = HelmUtils.getYamlObjectMapper();
    }

    @NonNull
    public HelmMetadata extract(RepositoryPath repositoryPath) {
        String path = repositoryPath.getTarget().toString();
        log.debug("Reading contents of helm chart '{}' and from repo '{}'", repositoryPath.getPath(),repositoryPath.getRepository().getId());
        try {
            InputStream in = Files.newInputStream(repositoryPath);
            try(FileInputStream fileInputStream = new FileInputStream(repositoryPath.getTarget().toString())) {
                InputStream uncompressedStream = new BufferedInputStream((InputStream) new GzipCompressorInputStream(fileInputStream));
                try {
                    ArchiveInputStream stream = (new ArchiveStreamFactory()).createArchiveInputStream(uncompressedStream);
                    try {
                        HelmMetadata helmMetadata = extractChartMetadata(path, stream);

                        stream.close();
                        uncompressedStream.close();
                        in.close();
                        return helmMetadata;
                    } catch (Throwable throwable) {

                        try {
                            stream.close();
                        } catch (Throwable throwable1) {
                            throwable.addSuppressed(throwable1);
                        }
                        throw throwable;
                    }
                } catch (Throwable throwable) {
                    try {
                        uncompressedStream.close();
                    } catch (Throwable throwable1) {
                        throwable.addSuppressed(throwable1);
                    }
                    throw throwable;
                }
            } catch (Throwable throwable) {

                try {
                    in.close();
                } catch (Throwable throwable1) {
                    throwable.addSuppressed(throwable1);
                }
                throw throwable;
            }
        } catch (Exception e) {
            log.error("Error while extracting metadata from {}: {}", repositoryPath.getPath(), e.getMessage());
            log.debug("Error while extracting metadata from chart: " + e.getMessage(), e);
            e.printStackTrace();
            return new HelmMetadata();
        }
    }

    private HelmMetadata extractChartMetadata(String path, ArchiveInputStream stream) throws IOException {
        HelmMetadataBuilder metadataBuilder = new HelmMetadataBuilder();
        ArchiveEntry entry;
        while ((entry = stream.getNextEntry()) != null) {
            if (stream.canReadEntryData(entry)) {
                extractFileMetadata(stream, metadataBuilder, entry, path);
                continue;
            }
            log.warn("Encountered unreadable archive entry '{}' inside archive {}", entry.getName(), path);
        }
        HelmMetadata helmMetadata = metadataBuilder.build();
        if (helmMetadata.helmChart == null)
            log.error("Could not extract metadata from chart {}", path);
        log.trace("Metadata extracted from {}", path);
        return helmMetadata;
    }

    private void extractFileMetadata(ArchiveInputStream stream, HelmMetadataBuilder metadataBuilder, ArchiveEntry entry, String path) throws IOException {
        File entryFile = new File(entry.getName());
        if (!entry.isDirectory() && !notRootChart(entryFile)) {
            if ("Chart.yaml".equals(entryFile.getName())) {
                log.debug("Found Chart.yaml file: {}", entry.getName());
                String readEntry = IOUtils.toString((InputStream) stream, Charset.defaultCharset());
                metadataBuilder.addChartCandidate(entry.getName(),
                        readFromChartYaml(readEntry));
            } else if ("requirements.yaml".equals(entryFile.getName())) {
                log.debug("Found requirements.yaml file: {}", entry.getName());
                String readEntry = IOUtils.toString((InputStream) stream, Charset.defaultCharset());
                metadataBuilder.setDependencies((readFromRequirementsYaml(readEntry, path)).dependencies);
            }
        }
    }

    private boolean notRootChart(File entryFile) {
        return (entryFile.getParentFile() != null && entryFile.getParentFile().getParentFile() != null);
    }

    private HelmChartMetadata readFromChartYaml(String entry) throws IOException {
        return (HelmChartMetadata) this.yamlObjectMapper.readValue(entry, HelmChartMetadata.class);
    }

    private HelmDependenciesMetadata readFromRequirementsYaml(String entry, String path) throws IOException {
        try {
            return (HelmDependenciesMetadata) this.yamlObjectMapper.readValue(entry, HelmDependenciesMetadata.class);
        } catch (MismatchedInputException e) {
            log.warn("Found requirements.yaml on path : {}. but couldn't extract dependencies metadata.", path);
            return new HelmDependenciesMetadata();
        }
    }
}

