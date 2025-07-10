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
package com.folib.components;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.folib.config.NpmLayoutProviderConfig;
import com.folib.constant.GlobalConstants;
import com.folib.enums.NpmPacketSuffix;
import com.folib.enums.NpmSubLayout;
import com.folib.npm.metadata.PackageFeed;
import com.folib.npm.metadata.PackageVersion;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.NpmLayoutProvider;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.javatuples.Pair;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import javax.inject.Inject;
import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/6/27
 **/
@Slf4j
@Component
public class NpmComponent {

    private static final String FIELD_NAME_LENGTH = "length";

    private static final String FIELD_NAME_ATTACHMENTS = "_attachments";

    private static final String FIELD_NAME_VERSION = "versions";

    @Inject
    @NpmLayoutProviderConfig.NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    public PackageFeed readPackageFeed(RepositoryPath repositoryPath) {
        PackageFeed packageFeed = null;
        String artifactPath = "";
        try {
            if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                return null;
            }
            artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            try (InputStream inputStream = Files.newInputStream(repositoryPath)) {
                packageFeed = npmJacksonMapper.readValue(inputStream, PackageFeed.class);
            }
        } catch (Exception ex) {
            log.warn("Npm storageId [{}] repositoryId [{}] artifactPath [{}] read packageFeed error [{}]", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, ExceptionUtils.getStackTrace(ex));
        }
        return packageFeed;
    }

    public void writePackageFeed(RepositoryPath repositoryPath, PackageFeed packageFeed) {
        if (Objects.isNull(packageFeed)) {
            return;
        }
        String artifactPath = "", lockName = String.format("npm-%s-package-feed-lock", packageFeed.getName());
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                if (Objects.isNull(repositoryPath)) {
                    return;
                }
                artifactPath = RepositoryFiles.relativizePath(repositoryPath);
                Files.createDirectories(repositoryPath.getParent());
//                npmJacksonMapper.enable(SerializationFeature.INDENT_OUTPUT);
                String packageFeedJson = npmJacksonMapper.writeValueAsString(packageFeed);
                Files.writeString(repositoryPath, packageFeedJson);
            } catch (Exception ex) {
                log.warn("Npm storageId [{}] repositoryId [{}] artifactPath [{}] write packageFeed error [{}]", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, ExceptionUtils.getStackTrace(ex));
            } finally {
                distributedLockComponent.unLock(lockName);
            }
        }
    }

    public PackageFeed convertToPackageFeed(RepositoryPath repositoryPath, String packageFeedData) {
        PackageFeed packageFeed = null;
        String artifactPath = "";
        if (StringUtils.isBlank(packageFeedData) || !JSONUtil.isJson(packageFeedData)) {
            return null;
        }
        try (InputStream inputStream = new ByteArrayInputStream(packageFeedData.getBytes())) {
            artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            packageFeed = npmJacksonMapper.readValue(inputStream, PackageFeed.class);
        } catch (Exception ex) {
            log.warn("Npm storageId [{}] repositoryId [{}] artifactPath [{}] packageFeedData [{}] convert to packageFeed error [{}]", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, packageFeedData, ExceptionUtils.getStackTrace(ex));
        }
        return packageFeed;
    }

    public PackageFeed convertToPackageFeed(String packageFeedData) {
        PackageFeed packageFeed = null;
        if (StringUtils.isBlank(packageFeedData) || !JSONUtil.isJson(packageFeedData)) {
            return null;
        }
        try (InputStream inputStream = new ByteArrayInputStream(packageFeedData.getBytes())) {
            packageFeed = npmJacksonMapper.readValue(inputStream, PackageFeed.class);
        } catch (Exception ex) {
            log.warn("Npm packageFeedData [{}] convert to packageFeed error [{}]", packageFeedData, ExceptionUtils.getStackTrace(ex));
        }
        return packageFeed;
    }

    private String extractPackageJson(InputStream in, String subLayout)
            throws IOException {
        GzipCompressorInputStream gzipIn = new GzipCompressorInputStream(in);
        try (TarArchiveInputStream tarIn = new TarArchiveInputStream(gzipIn)) {
            TarArchiveEntry entry;
            while ((entry = (TarArchiveEntry) tarIn.getNextEntry()) != null) {
                String packageJsonPath = NpmSubLayout.OHPM.getValue().equals(subLayout) ?
                        NpmLayoutProvider.OHPM_PACKAGE_JSON_PATH :
                        NpmLayoutProvider.DEFAULT_PACKAGE_JSON_PATH;
                if (!entry.getName().equals(packageJsonPath)) {
                    continue;
                }
                StringWriter writer = new StringWriter();
                IOUtils.copy(tarIn, writer, StandardCharsets.UTF_8);
                return writer.toString();
            }
            return null;
        }
    }

    public Path extractPackageJson(Path packageTgzTmp, String npmSubLayout, PackageVersion packageDef)
            throws IOException {
        String packageJsonSource;
        try (InputStream packageTgzIn = new BufferedInputStream(Files.newInputStream(packageTgzTmp))) {
            packageJsonSource = extractPackageJson(packageTgzIn, npmSubLayout);
        }
        String packageName = NpmSubLayout.OHPM.getValue().equals(npmSubLayout) ? "oh-package.json5" : "package";
        String suffix = NpmSubLayout.OHPM.getValue().equals(npmSubLayout) ? "json5" : "json";
        Path packageJsonTmp = Files.createTempFile(packageName, suffix);
        PackageVersion packageVersion = null;

        if (NpmSubLayout.OHPM.getValue().equals(npmSubLayout)) {
            try {
                assert packageJsonSource != null;
                try (InputStream inputStream = new ByteArrayInputStream(packageJsonSource.getBytes())) {
                    packageVersion = npmJacksonMapper.readValue(inputStream, PackageVersion.class);
                    packageVersion.setOhpmVersion(packageDef.getOhpmVersion());
                }
            } catch (IOException ex) {
                log.error("extractPackageJson  to ohpm version error [{}]", ExceptionUtils.getStackTrace(ex));
            }
            npmJacksonMapper.enable(SerializationFeature.INDENT_OUTPUT);
            packageJsonSource = npmJacksonMapper.writeValueAsString(packageVersion);
        }
        assert packageJsonSource != null;
        Files.write(packageJsonTmp, packageJsonSource.getBytes(StandardCharsets.UTF_8),
                StandardOpenOption.TRUNCATE_EXISTING);
        return packageJsonTmp;
    }

    public PackageVersion extractPackageVersion(String packageName,
                                                String packageJsonSource)
            throws IOException {
        PackageVersion packageVersion;
        try {
            packageVersion = npmJacksonMapper.readValue(packageJsonSource, PackageVersion.class);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Failed to parse package.json info for [%s]", packageName),
                    e);
        }
        Assert.isTrue(packageName.equals(packageVersion.getName()),
                String.format("Package name [%s] don't match with [%s].", packageVersion.getName(), packageName));

        return packageVersion;
    }

    private void moveToAttachment(JsonParser jp,
                                  String packageAttachmentName)
            throws IOException {
        Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT,
                String.format(
                        "Failed to parse npm package source for [%s], illegal attachment content type [%s].",
                        packageAttachmentName, jp.currentToken().name()));

        jp.nextToken();
        String contentType = jp.nextTextValue();
        Assert.isTrue(MediaType.APPLICATION_OCTET_STREAM_VALUE.equals(contentType),
                String.format("Failed to parse npm package source for [%s], unknown content type [%s]",
                        packageAttachmentName, contentType));

        String dataFieldName = jp.nextFieldName();
        Assert.isTrue("data".equals(dataFieldName),
                String.format("Failed to parse npm package source for [%s], data not found",
                        packageAttachmentName));

        jp.nextToken();
    }

    public static void unTarGz(String tarGzFilePath, String destDirectory) throws IOException {
        File destDir = new File(destDirectory);
        if (!destDir.exists()) {
            destDir.mkdirs();
        }
        try (InputStream fis = new BufferedInputStream(Files.newInputStream(Path.of(tarGzFilePath)));
             GzipCompressorInputStream gcis = new GzipCompressorInputStream(fis);
             TarArchiveInputStream tais = new TarArchiveInputStream(gcis)) {
            TarArchiveEntry entry;
            while ((entry = tais.getNextTarEntry()) != null) {
                File outputFile = new File(destDir, entry.getName());
                if (entry.isDirectory()) {
                    if (!outputFile.exists()) {
                        outputFile.mkdirs();
                    }
                } else {
                    // Ensure parent directory exists
                    File parent = outputFile.getParentFile();
                    if (!parent.exists()) {
                        parent.mkdirs();
                    }
                    try (OutputStream os = new FileOutputStream(outputFile)) {
                        byte[] buffer = new byte[4096];
                        int len;
                        while ((len = tais.read(buffer)) != -1) {
                            os.write(buffer, 0, len);
                        }
                    }
                }
            }
        }
    }

    private void addFilesToTar(String sourceDir, String currentPath, TarArchiveOutputStream tarOut) throws IOException {
        File file = new File(sourceDir, currentPath);
        String[] files = file.list();
        if (files != null) {
            for (String fileName : files) {
                File f = new File(file, fileName);
                String entryName = currentPath + "/" + f.getName();
                ArchiveEntry entry = tarOut.createArchiveEntry(f, entryName);
                tarOut.putArchiveEntry(entry);
                if (f.isDirectory()) {
                    tarOut.closeArchiveEntry();
                    addFilesToTar(sourceDir, entryName, tarOut);
                } else {
                    try (FileInputStream fis = new FileInputStream(f)) {
                        byte[] buffer = new byte[1024];
                        int len;
                        while ((len = fis.read(buffer)) != -1) {
                            tarOut.write(buffer, 0, len);
                        }
                    }
                    tarOut.closeArchiveEntry();
                }
            }
        }
    }

    public void createHar(String sourceDirectory, String harFilePath) throws IOException {
        try (FileOutputStream fos = new FileOutputStream(harFilePath);
             GzipCompressorOutputStream gcos = new GzipCompressorOutputStream(fos);
             TarArchiveOutputStream tarOut = new TarArchiveOutputStream(gcos)) {
            addFilesToTar(sourceDirectory, "", tarOut);
        }
    }

    private void changeHar(Path source, String ohpmVersion) throws IOException {
        // 1. 解压到临时目录
        File tempDir = Files.createTempDirectory("har-extract").toFile();
        unTarGz(source.toFile().getPath(), tempDir.getPath());
        JSONObject version = null;
        try (InputStream inputStream = Files.newInputStream(Path.of(tempDir.getPath(), "/package/oh-package.json5"))) {
            version = npmJacksonMapper.readValue(inputStream, JSONObject.class);
        }
        if (version == null){
            throw new IllegalArgumentException("oh-package.json5 not found");
        }
        version.put("_ohpmVersion", ohpmVersion);
        // 2. 修改文件
        File jsonFile = new File(tempDir, "/package/oh-package.json5");
        if (jsonFile.exists()) {
            npmJacksonMapper.enable(SerializationFeature.INDENT_OUTPUT);
            FileUtils.writeStringToFile(jsonFile, npmJacksonMapper.writeValueAsString(version), StandardCharsets.UTF_8);
        }
        // 3. 重新压缩
        createHar(tempDir.getPath(), source.toFile().getPath());
        FileUtils.deleteDirectory(tempDir);
    }

    private Path extractPackage(JsonParser jp, String subLayout, String ohpmVersion)
            throws IOException {
        final String suffix = NpmSubLayout.NPM.getValue().equals(subLayout) ? NpmPacketSuffix.TGZ.getValue() : NpmPacketSuffix.HAR.getValue();
        Path packageTgzTmp = Files.createTempFile("package", suffix);
        try (OutputStream packageTgzOut = new BufferedOutputStream(Files.newOutputStream(packageTgzTmp,
                StandardOpenOption.TRUNCATE_EXISTING))) {
            jp.readBinaryValue(packageTgzOut);
        }
        if (NpmSubLayout.OHPM.getValue().equals(subLayout)) {
            changeHar(packageTgzTmp, ohpmVersion);
        }
        long packageSize = Files.size(packageTgzTmp);
        Assert.isTrue(FIELD_NAME_LENGTH.equals(jp.nextFieldName()), "Failed to validate package content length.");
        jp.nextToken();
        if (!NpmSubLayout.OHPM.getValue().equals(subLayout)) {
            Assert.isTrue(packageSize == jp.getLongValue(), "Invalid package content length.");
        }
        jp.nextToken();
        return packageTgzTmp;
    }

    public Pair<PackageVersion, Path> extractPackage(String packageName,
                                                     InputStream in, String subLayout)
            throws IOException {
        Path packageSourceTmp = Files.createTempFile("package", "source");
        Files.copy(in, packageSourceTmp, StandardCopyOption.REPLACE_EXISTING);
        PackageVersion packageVersion = null;
        Path packageTgzPath = null;
        JsonFactory jsonFactory = new JsonFactory();
        try (InputStream tmpIn = new BufferedInputStream(Files.newInputStream(packageSourceTmp));
             JsonParser jp = jsonFactory.createParser(tmpIn);) {
            jp.setCodec(npmJacksonMapper);
            Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT, "npm package source should be JSON object.");
            while (jp.nextToken() != null) {
                String fieldName = jp.getCurrentName();
                // read value
                if (fieldName == null) {
                    continue;
                }
                switch (fieldName) {
                    case FIELD_NAME_VERSION:
                        jp.nextValue();
                        JsonNode node = jp.readValueAsTree();
                        Assert.isTrue(node.size() == 1, "npm package source should contain only one version.");

                        JsonNode packageJsonNode = node.iterator().next();
                        packageVersion = extractPackageVersion(packageName, packageJsonNode.toString());

                        break;
                    case FIELD_NAME_ATTACHMENTS:
                        Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT,
                                String.format(
                                        "Failed to parse npm package source for illegal type [%s] of attachment.",
                                        jp.currentToken().name()));

                        String packageAttachmentName = jp.nextFieldName();
                        log.info(String.format("Found npm package attachment [%s]", packageAttachmentName));

                        moveToAttachment(jp, packageAttachmentName);
                        String ohpmVersion = NpmSubLayout.OHPM.getValue().equals(subLayout) ? packageVersion.getOhpmVersion() : null;
                        packageTgzPath = extractPackage(jp, subLayout, ohpmVersion);
                        jp.nextToken();
                        jp.nextToken();
                        break;
                    default:
                }
            }
        }
        Files.delete(packageSourceTmp);
        if (packageVersion == null || packageTgzPath == null) {
            throw new IllegalArgumentException(
                    String.format("Failed to parse npm package source for [%s], attachment not found", packageName));
        }
        return Pair.with(packageVersion, packageTgzPath);
    }

    public String handleBinary(String repositoryBaseUrl, String data) {
        JSONArray jsonArray = JSONArray.parseArray(data);
        JSONObject jsonObject;
        String key = "url";
        URL url;
        for (int i = 0; i < jsonArray.size(); i++) {
            try {
                jsonObject = jsonArray.getJSONObject(i);
                if (jsonObject.containsKey(key)) {
                    url = new URL(jsonObject.getString(key));
                    jsonObject.put(key, repositoryBaseUrl + StringUtils.removeStart(url.getPath(), GlobalConstants.SEPARATOR));
                }
            } catch (Exception ex) {
                log.warn(ExceptionUtils.getStackTrace(ex));
            }
        }
        return jsonArray.toJSONString();
    }

    public String readBinary(RepositoryPath repositoryPath) {
        String artifactPath = "", data = "";
        try {
            if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                return null;
            }
            artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            data = Files.readString(repositoryPath);
        } catch (Exception ex) {
            log.warn("Npm storageId [{}] repositoryId [{}] artifactPath [{}] read binary error [{}]", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, ExceptionUtils.getStackTrace(ex));
        }
        return data;
    }

    public void writeBinary(RepositoryPath repositoryPath, String packageName, String data) {
        if (StringUtils.isBlank(data)) {
            return;
        }
        String artifactPath = "", lockName = String.format("npm-binary-%s-package-feed-lock", packageName);
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                if (Objects.isNull(repositoryPath)) {
                    return;
                }
                artifactPath = RepositoryFiles.relativizePath(repositoryPath);
                Files.createDirectories(repositoryPath.getParent());
                Files.writeString(repositoryPath, data);
            } catch (Exception ex) {
                log.warn("Npm storageId [{}] repositoryId [{}] artifactPath [{}] write binary error [{}]", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, ExceptionUtils.getStackTrace(ex));
            } finally {
                distributedLockComponent.unLock(lockName);
            }
        }
    }
}
