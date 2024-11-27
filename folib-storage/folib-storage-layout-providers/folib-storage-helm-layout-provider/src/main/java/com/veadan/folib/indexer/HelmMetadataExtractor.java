package com.veadan.folib.indexer;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;

import com.veadan.folib.model.HelmChartMetadata;
import com.veadan.folib.model.HelmDependenciesMetadata;
import com.veadan.folib.model.HelmMetadata;
import com.veadan.folib.model.HelmMetadataBuilder;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.util.HelmUtils;
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

