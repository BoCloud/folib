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
package com.folib.services.impl;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.artifact.coordinates.ConanArtifactIndex;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.ConanRevisions;
import com.folib.domain.SearchResults;
import com.folib.enums.ConanSearchRepositoryTypeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.services.ConanProvider;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class ConanHostedProvider implements ConanProvider {

    @Inject
    private ConanProviderRegistry conanProviderRegistry;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register() {
        conanProviderRegistry.addProvider(ConanSearchRepositoryTypeEnum.CONAN_HOSTED.getType(), this);
        log.info("Registered conan provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), ConanSearchRepositoryTypeEnum.CONAN_HOSTED.getType());
    }

    @Override
    public SearchResults search(String version, Repository repository, String query) {
        if (StringUtils.isNotBlank(query) && query.contains(GlobalConstants.ASTERISK)) {
            query = query.replaceAll("\\*", ".*");
        }
        SearchResults searchResults = SearchResults.builder().results(Lists.newArrayList()).build();
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId());
        try (Stream<Path> pathStream = Files.walk(rootRepositoryPath)) {
            String finalQuery = query;
            pathStream.filter(item -> !Files.isDirectory(item) && ConanArtifactIndex.isReferenceIndexJSON(item))
                    .sorted()
                    .forEach(item -> {
                        try {
                            ConanRevisions conanRevisions = JSONObject.parseObject(Files.readString(item), ConanRevisions.class);
                            if (Objects.nonNull(conanRevisions) && StringUtils.isNotBlank(conanRevisions.getReference())) {
                                boolean flag = StringUtils.isBlank(finalQuery) || (StringUtils.isNotBlank(finalQuery) && (conanRevisions.getReference().startsWith(finalQuery) || Pattern.matches(finalQuery, conanRevisions.getReference())));
                                if (flag) {
                                    searchResults.getResults().add(conanRevisions.getReference());
                                }
                            }
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    });
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return searchResults;
    }

    @Override
    public JSONObject revisionsSearch(Repository repository, String artifactPath, String url) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        String conaninfo = "conaninfo.txt";
        JSONObject data = new JSONObject();
        try (Stream<Path> pathStream = Files.walk(repositoryPath)) {
            pathStream.filter(item -> !Files.isDirectory(item) && item.getFileName().toString().equals(conaninfo))
                    .sorted()
                    .forEach(item -> {
                        try {
                            JSONObject subData = new JSONObject();
                            subData.put("requires", new JSONArray());
                            String content = Files.readString(item);
                            subData.put("content", content);
                            resolveContent(subData, content);
                            data.put(item.getParent().getFileName().toString(), subData);
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    });
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return data;
    }

    @Override
    public JSONObject revisions(Repository repository, String artifactPath, String targetUrl) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        try {
            String data = Files.readString(repositoryPath);
            return JSONObject.parseObject(data);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public JSONObject getLocalRevisions(Repository repository, String artifactPath, String targetUrl) {
        return revisions(repository, artifactPath, targetUrl);
    }

    @Override
    public JSONObject downloadUrls(Repository repository, String name, String version, String user, String channel) {
        String artifactPath = String.format("%s/%s/%s/%s/0/export", user, name, version, channel);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        JSONObject data = new JSONObject();
        String baseUrl = getRepositoryBaseUrl(repository);
        try (Stream<Path> pathStream = Files.walk(repositoryPath)) {
            pathStream.filter(item -> {
                try {
                    return RepositoryFiles.isArtifact((RepositoryPath) item);
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
                return false;
            })
                    .sorted()
                    .forEach(item -> {
                        try {
                            String filename = item.getFileName().toString();
                            data.put(filename, String.format("%s/v1/files/%s/%s/%s/%s/0/export/%s", baseUrl, user, name, version, channel, filename));
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    });
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return data;
    }

    @Override
    public JSONObject packageDownloadUrls(Repository repository, String name, String version, String user, String channel, String packageId) {
        String artifactPath = String.format("%s/%s/%s/%s/0/package/%s/0", user, name, version, channel, packageId);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        JSONObject data = new JSONObject();
        String baseUrl = getRepositoryBaseUrl(repository);
        try (Stream<Path> pathStream = Files.walk(repositoryPath)) {
            pathStream.filter(item -> {
                try {
                    return RepositoryFiles.isArtifact((RepositoryPath) item);
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
                return false;
            })
                    .sorted()
                    .forEach(item -> {
                        try {
                            String filename = item.getFileName().toString();
                            data.put(filename, String.format("%s/v1/files/%s/%s/%s/%s/0/package/%s/0/%s", baseUrl, user, name, version, channel, packageId, filename));
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    });
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return data;
    }

    @Override
    public JSONObject digest(Repository repository, String name, String version, String user, String channel) {
        String path = String.format("%s/%s/%s/%s/0/export/conanmanifest.txt", user, name, version, channel);
        return commonDigest(repository, path);
    }

    @Override
    public JSONObject packageDigest(Repository repository, String name, String version, String user, String channel, String packageId) {
        String path = String.format("%s/%s/%s/%s/0/package/%s/0/conanmanifest.txt", user, name, version, channel, packageId);
        return commonDigest(repository, path);
    }

    @Override
    public JSONObject getPackageInfo(Repository repository, String name, String version, String user, String channel, String packageId, String url) {
        String artifactPath = String.format("%s/%s/%s/%s/0/package/%s/0/", user, name, version, channel, packageId);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        JSONObject data = new JSONObject();
        LayoutFileSystemProvider provider = repositoryPath.getFileSystem().provider();
        try (Stream<Path> pathStream = Files.walk(repositoryPath)) {
            pathStream.filter(item -> {
                try {
                    return RepositoryFiles.isArtifact((RepositoryPath) item);
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
                return false;
            })
                    .sorted()
                    .forEach(item -> {
                        try {
                            final RepositoryPath checksumPath = provider.getChecksumPath((RepositoryPath) item, MessageDigestAlgorithms.MD5);
                            if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                                String filename = item.getFileName().toString();
                                data.put(filename, Files.readString(checksumPath));
                            }
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    });
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return data;
    }

    private JSONObject commonDigest(Repository repository, String path) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), path);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        JSONObject data = new JSONObject();
        String exportManifestUrl = getRepositoryBaseUrl(repository) + "/v1/files/" + path;
        data.put("conanmanifest.txt", exportManifestUrl);
        return data;
    }

    private void resolveContent(JSONObject data, String content) {
        // 使用正则表达式匹配带括号的键
        final String regex = "\\[(.*?)\\]";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(content);
        // 循环匹配键
        while (matcher.find()) {
            String key = matcher.group(1);
            data.put(key, getPackageInfo(content, "[" + key + "]"));
        }
    }

    private static Map<String, String> getPackageInfo(String content, String key) {
        if (StringUtils.isBlank(content)) {
            return null;
        }
        List<String> requiresKeyList = Lists.newArrayList("[full_requires]", "[requires]");
        boolean flag = false;
        Map<String, String> map = Maps.newLinkedHashMap();
        String[] lines = content.split("\\r?\\n");
        for (String line : lines) {
            if (key.equalsIgnoreCase(line.trim())) {
                flag = true;
                continue;
            } else if (line.trim().startsWith("[")) {
                flag = false;
            }
            if (flag && StringUtils.isNotBlank(line.trim())) {
                if (requiresKeyList.stream().anyMatch(item -> item.equalsIgnoreCase(key))) {
                    map.put(line, "");
                    continue;
                }
                String[] keyValue = line.split("=", 2);
                if (keyValue.length == 2) {
                    String itemKey = keyValue[0].trim();
                    String itemValue = keyValue[1].trim();
                    map.put(itemKey, itemValue);
                }
            }
        }
        return map;
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

}
