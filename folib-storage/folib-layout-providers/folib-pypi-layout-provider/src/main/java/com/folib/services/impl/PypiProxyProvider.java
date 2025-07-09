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

import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.PypiBrowsePackageHtmlResponseBuilder;
import com.folib.components.StorageClientComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.constants.PypiConstants;
import com.folib.domain.client.ResponseResult;
import com.folib.enums.PypiRepositoryTypeEnum;
import com.folib.enums.ResponseDataTypeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.pypi.PypiSearchResult;
import com.folib.services.PypiProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import com.folib.util.PypiUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Attribute;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PypiProxyProvider implements PypiProvider {

    @Inject
    private PypiProviderRegistry pypiProviderRegistry;

    @Inject
    private StorageClientComponent clientComponent;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private PypiBrowsePackageHtmlResponseBuilder pypiBrowsePackageHtmlResponseBuilder;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @PostConstruct
    @Override
    public void register() {
        pypiProviderRegistry.addProvider(PypiRepositoryTypeEnum.PYPI_PROXY.getType(), this);
        log.info("Registered pypi provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), PypiRepositoryTypeEnum.PYPI_PROXY.getType());
    }

    @Override
    public String packages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PypiUtils.getRemotePackageIndexPath(packageName);
        RepositoryPath packageHtmlRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            String htmlData = null;
            if (Objects.isNull(packageHtmlRepositoryPath) || !Files.exists(packageHtmlRepositoryPath) || RepositoryFiles.hasRefreshContent(packageHtmlRepositoryPath)) {
                if (RepositoryFiles.hasRefreshContent(packageHtmlRepositoryPath)) {
                    log.info("Pypi indexJsonRepositoryPath [{}] [{}] [{}] refresh content", packageHtmlRepositoryPath.getStorageId(), packageHtmlRepositoryPath.getRepositoryId(), packageName);
                }
                htmlData = commonUrlJSONData(repository, targetUrl);
                if (StringUtils.isBlank(htmlData)) {
                    if (Files.exists(packageHtmlRepositoryPath)) {
                        return Files.readString(packageHtmlRepositoryPath);
                    }
                    return null;
                }
                try {
                    String storageId = repository.getStorage().getId();
                    String repositoryId = repository.getId();
                    String packageTargetUrl = "";
                    RemoteRepository remoteRepository = repository.getRemoteRepository();
                    if (remoteRepository.getUrl().endsWith(GlobalConstants.SEPARATOR)) {
                        packageTargetUrl = String.format("%s%s", remoteRepository.getUrl(), packageName);
                    } else {
                        packageTargetUrl = String.format("%s/%s", remoteRepository.getUrl(), packageName);
                    }
                    String prefix = "";
                    if (packageTargetUrl.contains("/storages/")) {
                        prefix = packageTargetUrl.substring(packageTargetUrl.indexOf("/storages/"), packageTargetUrl.indexOf("/simple/"));
                        if (!prefix.endsWith(GlobalConstants.SEPARATOR)) {
                            prefix = prefix + GlobalConstants.SEPARATOR;
                        }
                    }
                    String finalPrefix = prefix;
                    Matcher matcher = PypiConstants.PACKAGE_NAME_PATTERN.matcher(htmlData);
                    String finalPackageTargetUrl = packageTargetUrl;
                    List<PypiSearchResult> pypiSearchResultList = matcher.results().map(matchResult -> handleVersion(storageId, repositoryId, finalPackageTargetUrl, finalPrefix, matchResult))
                            .filter(Objects::nonNull).collect(Collectors.toList());
                    Files.createDirectories(packageHtmlRepositoryPath.getParent());
                    Files.writeString(packageHtmlRepositoryPath, pypiBrowsePackageHtmlResponseBuilder.getProxyHtmlResponse(pypiSearchResultList));
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
            return Files.readString(packageHtmlRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public String getLocalPackages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PypiUtils.getRemotePackageIndexPath(packageName);
        RepositoryPath packageHtmlRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            if (Objects.nonNull(packageHtmlRepositoryPath) && Files.exists(packageHtmlRepositoryPath) && !RepositoryFiles.hasRefreshContent(packageHtmlRepositoryPath)) {
                return Files.readString(packageHtmlRepositoryPath);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private PypiSearchResult handleVersion(String storageId, String repositoryId, String packageTargetUrl, String finalPrefix, MatchResult matchResult) {
        String artifactName = "", artifactUrl = "";
        try {
            artifactName = matchResult.group(2);
            artifactUrl = matchResult.group(1);
            if (StringUtils.isNotBlank(finalPrefix) && artifactUrl.contains(finalPrefix)) {
                artifactUrl = artifactUrl.replace(finalPrefix, "/../../");
            }
            String artifactPath = artifactUrl.substring(artifactUrl.indexOf("/packages/") + "/packages/".length());
            artifactUrl = PypiUtils.resolveUrl(packageTargetUrl, artifactUrl);
            return PypiSearchResult.builder().artifactName(artifactName).artifactPath(artifactPath).artifactUrl(artifactUrl).storageId(storageId).repositoryId(repositoryId).groupName(PypiCoordinates.parse(artifactName).getId()).attributes(getAttributes(matchResult.group(0))).build();
        } catch (Exception ex) {
            log.error("Pypi storageId [{}] repositoryId [{}] packageName [{}] parse error [{}]", storageId, repositoryId, artifactName, ExceptionUtils.getStackTrace(ex));
//            throw new RuntimeException(ex.getMessage());
        }
        return null;
    }

    private String commonUrlJSONData(Repository repository, String url) {
        String data = null;
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", repository.getStorage().getId(), repository.getId(), remoteRepository.getUrl());
            return null;
        }
        String prefixUrl = remoteRepository.getUrl();
        String suffixUrl = url;
        if (!suffixUrl.startsWith(GlobalConstants.SEPARATOR)) {
            suffixUrl = GlobalConstants.SEPARATOR + suffixUrl;
        }
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        ResponseResult responseResult = clientComponent.doGet(repository.getStorage().getId(), repository.getId(), targetUrl);
        if (Objects.isNull(responseResult)) {
            return null;
        }
        if (HttpStatus.OK.value() == responseResult.getHttpStatus() && ResponseDataTypeEnum.STRING.equals(responseResult.getDataType()) && StringUtils.isNotBlank(responseResult.getData())) {
            data = responseResult.getData();
        }
        return data;
    }

    private String getAttributes(String html) {
        try {
            if (StringUtils.isBlank(html)) {
                return "";
            }
            List<String> includeAttributes = getIncludeAttributes();
            StringBuffer attributesStringBuffer = new StringBuffer();
            // 解析HTML字符串
            Document doc = Jsoup.parse(html);
            // 选择所有<a>标签
            Elements aTags = doc.select("a");
            // 遍历每个<a>标签并获取属性
            for (Element aTag : aTags) {
                // 获取所有属性
                for (Attribute attribute : aTag.attributes().asList()) {
                    if (includeAttributes.stream().noneMatch(item -> item.equalsIgnoreCase(attribute.getKey()))) {
                        continue;
                    }
                    attributesStringBuffer.append(attribute.getKey());
                    attributesStringBuffer.append("=\"");
                    attributesStringBuffer.append(StringEscapeUtils.escapeXml(attribute.getValue()));
                    attributesStringBuffer.append("\"");
                    attributesStringBuffer.append(" ");
                }
            }
            return attributesStringBuffer.toString();
        } catch (Exception ex) {
            log.error("Pypi proxy index html [{}] error [{}]", html, ExceptionUtils.getStackTrace(ex));
        }
        return "";
    }

    private List<String> getIncludeAttributes() {
        List<String> includeAttributes = Lists.newArrayList("data-requires-python");
        String key = "PYPI_INCLUDE_ATTRIBUTES";
        String cacheValue = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(cacheValue)) {
            includeAttributes = Arrays.asList(cacheValue.split(","));
        }
        return includeAttributes;
    }
}
