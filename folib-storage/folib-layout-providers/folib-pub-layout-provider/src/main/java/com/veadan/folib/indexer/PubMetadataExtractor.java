package com.veadan.folib.indexer;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.constants.PubConstants;
import com.veadan.folib.domain.Pubspec;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.utils.PubUtils;
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

