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
package com.folib.controllers.layout.php;


import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.folib.artifact.coordinates.PhpCoordinates;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.controllers.BaseArtifactController;
import com.folib.data.criteria.Paginator;
import com.folib.php.PhpSearchPackage;
import com.folib.php.PhpSearchRequest;
import com.folib.php.PhpSearchResult;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.PhpLayoutProvider;
import com.folib.providers.PhpSearchResultSupplier;
import com.folib.providers.repository.RepositoryProvider;
import com.folib.providers.repository.RepositoryProviderRegistry;
import com.folib.providers.repository.RepositorySearchRequest;
import com.folib.repository.PhpRepositoryFeatures;
import com.folib.storage.repository.Repository;
import com.folib.web.Constants;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@RestController
@LayoutReqMapping(PhpLayoutProvider.ALIAS)
@Slf4j
@Api(description = "php坐标控制器",tags = "php坐标控制器")
public class PhpArtifactController extends BaseArtifactController {

    private final static String DEFAULT_URL = "https://packagist.org/search.json";

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private PhpSearchResultSupplier phpSearchResultSupplier;

    @Inject
    private PhpRepositoryFeatures.PhpSearchPackagesEventListener phpSearchPackagesEventListener;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactComponent artifactComponent;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @GetMapping(path = "{storageId}/{repositoryId}/search")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void search(@RepoMapping Repository repository,
                       @RequestParam(name = "q") String q,
                       @RequestParam(name = "type") String type,
                       @RequestParam(name = "size", defaultValue = "20") Integer size,
                       HttpServletResponse response)
            throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, PhpCoordinates.DEFAULT_PACKAGES);
        JSONObject packageJson = JSONObject.parseObject(artifactComponent.readRepositoryPathContent(repositoryPath));
        PhpSearchRequest phpSearchRequest = new PhpSearchRequest();
        phpSearchRequest.setQ(q);
        phpSearchRequest.setType(type);
        String searchKey = "search", targetUrl = DEFAULT_URL;
        if (packageJson.containsKey(searchKey)) {
            targetUrl = packageJson.getString(searchKey);
            phpSearchRequest.setTargetUrl(targetUrl.substring(0, targetUrl.lastIndexOf("?")));
        }
        phpSearchRequest.setSize(size);

        phpSearchPackagesEventListener.setPhpSearchRequest(phpSearchRequest);

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());

        RepositorySearchRequest predicate = new RepositorySearchRequest(q, Collections.singleton("json"));
        Paginator paginator = new Paginator();
        paginator.setLimit(20);
        List<Path> searchResult = provider.search(storageId, repositoryId, predicate, paginator);

        PhpSearchResult phpSearchResult = new PhpSearchResult();
        List<PhpSearchPackage> resultList = Lists.newArrayList();
        searchResult.stream().map(phpSearchResultSupplier).forEach(resultList::add);
        Long count = provider.count(storageId, repositoryId, predicate);
        phpSearchResult.setTotal(count.intValue());
        phpSearchResult.setResults(resultList);
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.getOutputStream().write(JSONObject.toJSONBytes(phpSearchResult));
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/dists/{path:.+}"})
    public ResponseEntity<Object> distDownload(@RepoMapping Repository repository,
                                               @RequestHeader HttpHeaders httpHeaders,
                                               @PathVariable String path,
                                               HttpServletRequest request,
                                               HttpServletResponse response)
            throws Exception {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = getLocalRepositoryPath(storageId, repositoryId, PhpCoordinates.DEFAULT_PACKAGES);
        storageId = repositoryPath.getStorageId();
        repositoryId = repositoryPath.getRepositoryId();
        JSONObject packageJson = getSourcePackagesJson(repositoryPath);
        String mirrorsKey = "mirrors";
        if (packageJson.containsKey(mirrorsKey)) {
            repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, PhpCoordinates.COMPOSER_DISTS + path);
        } else {
            PhpCoordinates phpArtifactCoordinates = PhpCoordinates.parse(path);
            String reference = path.substring(path.lastIndexOf("/"), path.lastIndexOf("."));
            String name = phpArtifactCoordinates.getName().replace(reference, "");
            String artifactPath = "%s%s.%s";
            artifactPath = String.format(artifactPath, PhpCoordinates.COMPOSER_P2, name, PhpCoordinates.JSON);
            String url = getTargetUrl(storageId, repositoryId, artifactPath, name, reference);
            if (StringUtils.isBlank(url)) {
                artifactPath = "%s%s~dev.%s";
                artifactPath = String.format(artifactPath, PhpCoordinates.COMPOSER_P2, name, PhpCoordinates.JSON);
                url = getTargetUrl(storageId, repositoryId, artifactPath, name, reference);
            }
            if (StringUtils.isNotBlank(url)) {
                repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, url, PhpCoordinates.COMPOSER_DISTS + path);
            }
        }
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        return null;
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/{path:.+}"})
    public ResponseEntity<Object> download(@RepoMapping Repository repository,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           @PathVariable String path,
                                           HttpServletRequest request,
                                           HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        if (PhpCoordinates.DEFAULT_PACKAGES.equals(path) && Objects.nonNull(repositoryPath)) {
            return ResponseEntity.ok(getPackagesJson(repositoryPath));
        }
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        return null;
    }

    /**
     * 获取原packages.json文件信息
     *
     * @param repositoryPath 路径
     * @return 原packages.json文件信息
     */
    private JSONObject getSourcePackagesJson(RepositoryPath repositoryPath) throws IOException {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return new JSONObject();
        }
        String packages = artifactComponent.readRepositoryPathContent(repositoryPath);
        return JSONObject.parseObject(packages);
    }

    /**
     * 获取packages.json文件信息
     *
     * @param repositoryPath 路径
     * @return packages.json文件信息
     * @throws IOException io异常
     */
    private JSONObject getPackagesJson(RepositoryPath repositoryPath) throws IOException {
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        String phpBaseUrl = "%s/%s/%s";
        phpBaseUrl = String.format(phpBaseUrl, Constants.ARTIFACT_ROOT_PATH, storageId, repositoryId);
        JSONObject packagesJson = getSourcePackagesJson(repositoryPath);
        String metadataUrlKey = "metadata-url", providersUrlKey = "providers-url", providersLazyUrlKey = "providers-lazy-url",
                searchKey = "search", mirrorsKey = "mirrors", distKey = "dists";
        if (packagesJson.containsKey(metadataUrlKey)) {
            packagesJson.put(metadataUrlKey, phpBaseUrl + "/p2/%package%.json");
        }
        if (packagesJson.containsKey(providersUrlKey)) {
            packagesJson.put(providersUrlKey, phpBaseUrl + "/p/%package%$%hash%.json");
        }
        if (packagesJson.containsKey(providersLazyUrlKey)) {
            packagesJson.put(providersLazyUrlKey, phpBaseUrl + "/p/%package%$%hash%.json");
        }
        String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();

        String pathUrl = "%s/%s/%s/%s";
        pathUrl = String.format(pathUrl, Constants.ARTIFACT_ROOT_PATH, storageId, repositoryId, distKey);
        String mirrors = "[" +
                "        {" +
                "            \"dist-url\": \"" + artifactComponent.escapeUrl(baseUrl, pathUrl + "/%package%/%reference%.%type%") + "\"," +
                "            \"preferred\": true" +
                "        }" +
                "    ]";
        packagesJson.put(mirrorsKey, JSONObject.parseArray(mirrors));

        String searchUrl = phpBaseUrl + "/search?q=%query%&type=%type%";
        searchUrl = artifactComponent.escapeUrl(baseUrl, searchUrl);
        packagesJson.put(searchKey, searchUrl);
        return packagesJson;
    }

    /**
     * 获取实际包地址，从索引文件xxx.json中解析出dist url
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactPath 路径
     * @param name         包名
     * @param reference    reference
     * @return 目标地址
     * @throws IOException io异常
     */
    private String getTargetUrl(String storageId, String repositoryId, String artifactPath, String name, String reference) throws IOException {
        String artifactContent = artifactComponent.readRepositoryPathContent(storageId, repositoryId, artifactPath);
        if (StringUtils.isBlank(artifactContent)) {
            return "";
        }
        JSONObject artifactJson = JSONObject.parseObject(artifactContent);
        JSONObject packagesJson = artifactJson.getJSONObject("packages");
        JSONArray packagesJsonArray = packagesJson.getJSONArray(name);
        JSONObject versionJson = null, distJson = null;
        String url = "", distReference, distKey = "dist", referenceKey = "reference", urlKey = "url";
        reference = reference.replace("/", "");
        for (int i = 0; i < packagesJsonArray.size(); i++) {
            versionJson = packagesJsonArray.getJSONObject(i);
            if (versionJson.containsKey(distKey)) {
                distJson = versionJson.getJSONObject(distKey);
                distReference = distJson.getString(referenceKey);
                if (distReference.equals(reference)) {
                    url = distJson.getString(urlKey);
                    break;
                }
            }
        }
        return url;
    }

    private RepositoryPath getLocalRepositoryPath(String storageId, String repositoryId, String artifactPath) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        repositoryPath.setDisableRemote(Boolean.TRUE);
        RepositoryPath cacheRepositoryPath = artifactResolutionService.resolvePath(repositoryPath);
        if (Objects.nonNull(cacheRepositoryPath) && Files.exists(cacheRepositoryPath)) {
            return cacheRepositoryPath;
        }
        repositoryPath.setDisableRemote(null);
        return repositoryPath;
    }
}