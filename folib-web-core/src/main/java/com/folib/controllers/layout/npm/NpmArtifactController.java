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
package com.folib.controllers.layout.npm;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.providers.*;
import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.authentication.api.password.PasswordAuthentication;
import com.folib.components.NpmComponent;
import com.folib.config.NpmLayoutProviderConfig.NpmObjectMapper;
import com.folib.constant.GlobalConstants;
import com.folib.controllers.BaseArtifactController;
import com.folib.data.criteria.Paginator;
import com.folib.enums.NpmPacketSuffix;
import com.folib.enums.NpmSubLayout;
import com.folib.model.request.OhpmLoginReq;
import com.folib.model.response.OhpmLoginRes;
import com.folib.model.response.OhpmPublishRes;
import com.folib.npm.NpmSearchRequest;
import com.folib.npm.NpmViewRequest;
import com.folib.npm.metadata.PackageFeed;
import com.folib.npm.metadata.PackageVersion;
import com.folib.npm.metadata.SearchResults;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.repository.RepositoryProvider;
import com.folib.providers.repository.RepositoryProviderRegistry;
import com.folib.providers.repository.RepositorySearchRequest;
import com.folib.repository.NpmRepositoryFeatures.SearchPackagesEventListener;
import com.folib.repository.NpmRepositoryFeatures.ViewPackageEventListener;
import com.folib.services.NpmService;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.javatuples.Pair;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;


/**
 * This Controller used to handle npm requests.
 *
 * @author @author veadan
 */
@RestController
@LayoutReqMapping(NpmCoordinates.LAYOUT_NAME)
@Api(description = "npm坐标控制器", tags = "npm坐标控制器")
public class NpmArtifactController
        extends BaseArtifactController {

    private static final String FIELD_NAME_LENGTH = "length";

    private static final String FIELD_NAME_ATTACHMENTS = "_attachments";

    private static final String FIELD_NAME_VERSION = "versions";

    @Inject
    @NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private NpmPackageSupplier npmPackageSupplier;

    @Inject
    private NpmSearchResultSupplier npmSearchResultSupplier;

    @Inject
    private ViewPackageEventListener viewPackageEventListener;

    @Inject
    private SearchPackagesEventListener searcPackagesEventListener;

    @Inject
    private NpmUnpublishService npmUnpublishService;

    @Inject
    private NpmService npmService;

    @Inject
    private NpmComponent npmComponent;

    @Inject
    protected ApplicationEventPublisher eventPublisher;

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private ThreadPoolTaskExecutor asyncFetchRemotePackageThreadPoolTaskExecutor;

    @Inject
    private AuthenticationManager authenticationManager;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @GetMapping(path = "{storageId}/{repositoryId}/-/v1/search")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void search(@RepoMapping Repository repository,
                       @RequestParam(name = "text") String text,
                       @RequestParam(name = "size", defaultValue = "20") Integer size,
                       HttpServletResponse response)
            throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        NpmSearchRequest npmSearchRequest = new NpmSearchRequest();
        npmSearchRequest.setText(text);
        npmSearchRequest.setSize(size);

        searcPackagesEventListener.setNpmSearchRequest(npmSearchRequest);

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());

        RepositorySearchRequest predicate = new RepositorySearchRequest(text, Collections.singleton("tgz"));
        Paginator paginator = new Paginator();
        paginator.setLimit(20);
        List<Path> searchResult = provider.search(storageId, repositoryId, predicate, paginator);

        SearchResults searchResults = new SearchResults();
        searchResult.stream().map(npmSearchResultSupplier).forEach(p -> {
            searchResults.getObjects().add(p);
        });

        Long count = provider.count(storageId, repositoryId, predicate);
        searchResults.setTotal(count.intValue());

        //Wed Oct 31 2018 05:01:19 GMT+0000 (UTC)
        SimpleDateFormat format = new SimpleDateFormat(NpmSearchResultSupplier.SEARCH_DATE_FORMAT);
        searchResults.setTime(format.format(new Date()));

        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.getOutputStream().write(npmJacksonMapper.writeValueAsBytes(searchResults));
    }

    @GetMapping(path = "{storageId}/{repositoryId}/-/binary/{artifactPath:.+}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewBinaryFeedWithScope(@RepoMapping Repository repository,
                                        @PathVariable(name = "storageId") String storageId,
                                        @PathVariable(name = "repositoryId") String repositoryId,
                                        @PathVariable(name = "artifactPath") String artifactPath,
                                        HttpServletRequest request,
                                        HttpServletResponse response,
                                        @RequestHeader HttpHeaders httpHeaders)
            throws Exception {
        String extension = FilenameUtils.getExtension(artifactPath);
        if (StringUtils.isNotBlank(extension)) {
            String prefix = String.format("/storages/%s/%s", storageId, repositoryId);
            String packageId = request.getRequestURI().substring(prefix.length() + 1);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, packageId);
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            return;
        }
        long startTime = System.currentTimeMillis();
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        String prefix = String.format("/storages/%s/%s", storageId, repositoryId);
        String packageId = request.getRequestURI().substring(prefix.length());
        String binary = "/binary/";
        String packageName = "binary";
        if (!packageId.endsWith(binary) && !packageId.endsWith(StringUtils.removeEnd(binary, GlobalConstants.SEPARATOR))) {
            packageName = StringUtils.removeEnd(packageId.substring(packageId.indexOf(binary) + binary.length()), GlobalConstants.SEPARATOR);
        }
        String binaryFeed = npmService.binary(repository, packageName, packageId);
        if (Objects.isNull(binaryFeed)) {
            String msg = "{\"error\":\"[NOT_FOUND] %s not found\"}";
            response.setStatus(HttpStatus.NOT_FOUND.value());
            response.getOutputStream().write(String.format(msg, packageId).getBytes());
            return;
        }
        try (InputStream inputStream = new ByteArrayInputStream(binaryFeed.getBytes(StandardCharsets.UTF_8))) {
            copyToResponse(inputStream, response);
        }
        logger.debug("[{}] viewPackageFeedWithScope storageId [{}] repositoryId [{}] packageId [{}] task time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), packageId, System.currentTimeMillis() - startTime);
    }

    @GetMapping(path = "{storageId}/{repositoryId}/{packageScope:[^-].+}/{packageName:[^-].+}/{packageVersion}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewPackageWithScope(@RepoMapping Repository repository,
                                     @PathVariable(name = "packageScope") String packageScope,
                                     @PathVariable(name = "packageName") String packageName,
                                     @PathVariable(name = "packageVersion") String packageVersion,
                                     HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        String packageId = NpmCoordinates.calculatePackageId(packageScope, packageName);
        final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        NpmCoordinates c = NpmCoordinates.of(packageId, packageVersion, packageSuffix);

        NpmViewRequest npmSearchRequest = new NpmViewRequest();
        npmSearchRequest.setPackageId(packageId);
        npmSearchRequest.setVersion(packageVersion);
        viewPackageEventListener.setNpmSearchRequest(npmSearchRequest);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, c.buildPath());
        if (repositoryPath == null) {
            response.setStatus(HttpStatus.NOT_FOUND.value());

            return;
        }

        NpmPackageDesc packageDesc = npmPackageSupplier.apply(repositoryPath);
        PackageVersion npmPackage = packageDesc.getNpmPackage();

        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.getOutputStream().write(npmJacksonMapper.writeValueAsBytes(npmPackage));
    }

    @GetMapping(path = "{storageId}/{repositoryId}/{packageScope:[^-].+}/{packageName}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewPackageFeedWithScope(@RepoMapping Repository repository,
                                         @PathVariable(name = "packageScope") String packageScope,
                                         @PathVariable(name = "packageName") String packageName,
                                         HttpServletRequest request,
                                         HttpServletResponse response,
                                         @RequestHeader HttpHeaders httpHeaders)
            throws Exception {
        long startTime = System.currentTimeMillis();
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        String packageId = NpmCoordinates.calculatePackageId(packageScope, packageName);
        PackageFeed packageFeed = npmService.packageFeed(repository, packageId, packageId);
        if (Objects.isNull(packageFeed)) {
            String msg = "{\"error\":\"[NOT_FOUND] %s not found\"}";
            response.setStatus(HttpStatus.NOT_FOUND.value());
            response.getOutputStream().write(String.format(msg, packageId).getBytes());
            return;
        }

        try (InputStream inputStream = new ByteArrayInputStream(npmJacksonMapper.writeValueAsBytes(packageFeed))) {
            copyToResponse(inputStream, response);
        }
        logger.debug("[{}] viewPackageFeedWithScope storageId [{}] repositoryId [{}] packageId [{}] task time [{}] ms", this.getClass().getSimpleName(), repository.getStorage().getId(), repository.getId(), packageId, System.currentTimeMillis() - startTime);
    }

    private String generateRevisionHashcode(PackageFeed packageFeed) {
        String versionsShasum = packageFeed.getVersions().getAdditionalProperties()
                .values()
                .stream()
                .map(x -> x.getDist().getShasum())
                .collect(Collectors.joining());
        return packageFeed.getVersions().getAdditionalProperties().size() + "-" +
                DigestUtils.sha1Hex(versionsShasum).substring(0, 16);
    }

    @GetMapping(path = "{storageId}/{repositoryId}/{packageName}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void viewPackageFeed(@RepoMapping Repository repository,
                                @PathVariable(name = "packageName") String packageName,
                                HttpServletRequest request,
                                HttpServletResponse response,
                                @RequestHeader HttpHeaders httpHeaders)
            throws Exception {
        viewPackageFeedWithScope(repository, null, packageName, request, response, httpHeaders);
    }

    private RepositorySearchRequest createSearchPredicate(String packageScope,
                                                          String packageName, String subLayout) {
        List<String> coordinateValues = NpmSubLayout.OHPM.getValue().equals(subLayout) ? Lists.newArrayList("har") : Lists.newArrayList("tgz");
        RepositorySearchRequest rootPredicate = new RepositorySearchRequest(
                NpmCoordinates.calculatePackageId(packageScope, packageName), Lists.newArrayList(coordinateValues));

        return rootPredicate;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{packageScope}/{packageName}/-/{packageNameWithVersion}.{packageExtension}",
            method = {RequestMethod.GET, RequestMethod.HEAD})
    public ResponseEntity<Object> downloadPackageWithScope(@RepoMapping Repository repository,
                                                           @PathVariable(name = "packageScope") String packageScope,
                                                           @PathVariable(name = "packageName") String packageName,
                                                           @PathVariable(name = "packageNameWithVersion") String packageNameWithVersion,
                                                           @PathVariable(name = "packageExtension") String packageExtension,
                                                           @RequestHeader HttpHeaders httpHeaders,
                                                           HttpServletRequest request,
                                                           HttpServletResponse response)
            throws Exception {
        long startTime = System.currentTimeMillis();
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String packageVersion = "";
        //Example of packageNameWithVersion  core-9.0.1-next.8.tgz
        boolean isPackage = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? packageNameWithVersion.startsWith("oh-package") && packageExtension.endsWith("json5") : packageNameWithVersion.startsWith("package-") && packageExtension.endsWith("json");
        String artifactPath = "";
        if (!isPackage) {
            if (!packageNameWithVersion.startsWith(packageName + "-")) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                return null;
            }
            packageVersion = getPackageVersion(packageNameWithVersion, packageName);
            NpmCoordinates coordinates;
            try {
                final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                coordinates = NpmCoordinates.of(String.format("%s/%s", packageScope, packageName), packageVersion, packageSuffix);
                artifactPath = coordinates.buildPath();
            } catch (IllegalArgumentException e) {
                response.setStatus(HttpStatus.BAD_REQUEST.value());
                response.getWriter().write(e.getMessage());
                return null;
            }
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            logger.debug("[{}] downloadPackageWithScope [{}] task time [{}] ms", this.getClass().getSimpleName(), artifactPath, System.currentTimeMillis() - startTime);
        } else {
            packageVersion = getPackageJsonVersion(packageNameWithVersion, repository.getSubLayout());
            String pgName = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmLayoutProvider.OH_PACKAGE_JSON : NpmLayoutProvider.PACKAGE_JSON;
            artifactPath = String.format("%s/%s/%s/%s", packageScope, packageName, packageVersion, pgName);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            String packages = npmComponent.readBinary(repositoryPath);
            GsonBuilder gsonBuilder = new GsonBuilder();
            gsonBuilder.setPrettyPrinting();
            Gson gson = gsonBuilder.create();
            return ResponseEntity.ok(gson.toJson(packages));
        }
        return null;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{packageName}/-/{packageNameWithVersion}.{packageExtension}",
            method = {RequestMethod.GET,
                    RequestMethod.HEAD})
    public ResponseEntity<Object> downloadPackage(@RepoMapping Repository repository,
                                                  @PathVariable(name = "packageName") String packageName,
                                                  @PathVariable(name = "packageNameWithVersion") String packageNameWithVersion,
                                                  @PathVariable(name = "packageExtension") String packageExtension,
                                                  @RequestHeader HttpHeaders httpHeaders,
                                                  HttpServletRequest request,
                                                  HttpServletResponse response)
            throws Exception {
        long startTime = System.currentTimeMillis();
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String packageVersion = "";
        //Example of packageNameWithVersion core-9.0.1-next.8.tgz
        boolean isPackage = packageNameWithVersion.startsWith("package-") && packageExtension.endsWith("json");
        String artifactPath = "";
        if (!isPackage) {
            if (!packageNameWithVersion.startsWith(packageName + "-")) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                return null;
            }
            packageVersion = getPackageVersion(packageNameWithVersion, packageName);

            NpmCoordinates coordinates;
            try {
                final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                coordinates = NpmCoordinates.of(packageName, packageVersion, packageSuffix);
            } catch (IllegalArgumentException e) {
                response.setStatus(HttpStatus.BAD_REQUEST.value());
                response.getWriter().write(e.getMessage());
                return null;
            }

            RepositoryPath path = artifactResolutionService.resolvePath(storageId, repositoryId, coordinates.buildPath());
            vulnerabilityBlock(path);
            provideArtifactDownloadResponse(request, response, httpHeaders, path);
            logger.debug("[{}] downloadPackage [{}] task time [{}] ms", this.getClass().getSimpleName(), coordinates.buildPath(), System.currentTimeMillis() - startTime);
        } else {
            packageVersion = getPackageJsonVersion(packageNameWithVersion, repository.getSubLayout());
            artifactPath = String.format("%s/%s/%s/%s", packageName, packageName, packageVersion, NpmLayoutProvider.PACKAGE_JSON);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            String packages = npmComponent.readBinary(repositoryPath);
            GsonBuilder gsonBuilder = new GsonBuilder();
            gsonBuilder.setPrettyPrinting();
            Gson gson = gsonBuilder.create();
            return ResponseEntity.ok(gson.toJson(packages));
        }
        return null;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(path = "{storageId}/{repositoryId}/{packageScope}/{packageName}/{packageVersion}/{fileName}.{fileExtension}")
    public void downloadPackageWithScopeFile(@RepoMapping Repository repository,
                                             @PathVariable(name = "packageScope") String packageScope,
                                             @PathVariable(name = "packageName") String packageName,
                                             @PathVariable(name = "packageVersion") String packageVersion,
                                             @PathVariable(name = "fileName") String fileName,
                                             @PathVariable(name = "fileExtension") String fileExtension,
                                             @RequestHeader HttpHeaders httpHeaders,
                                             HttpServletRequest request,
                                             HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String path = packageScope + File.separator + packageName + File.separator + packageVersion + File.separator + fileName + GlobalConstants.POINT + fileExtension;
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(path = "{storageId}/{repositoryId}/{name:.+}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity publish(@RepoMapping Repository repository,
                                  @PathVariable(name = "name") String name,
                                  HttpServletRequest request)
            throws Exception {
        if (nameContainsRevision(name)) {
            return ResponseEntity.status(HttpStatus.OK).build();
        }
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final String subLayout = repository.getSubLayout();

        logger.info("npm publish request for {}/{}/{}", storageId, repositoryId, name);
        Pair<PackageVersion, Path> packageEntry;
        try {
            packageEntry = npmComponent.extractPackage(name, request.getInputStream(), subLayout);
        } catch (IllegalArgumentException e) {
            logger.error("Failed to extract npm package data", e);
            return ResponseEntity.badRequest().build();
        }

        final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(subLayout) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        PackageVersion packageJson = packageEntry.getValue0();
        Path packageTgz = packageEntry.getValue1();
        NpmCoordinates coordinates = NpmCoordinates.of(name, packageJson.getVersion(), packageSuffix);
        storeNpmPackage(repository, coordinates, packageJson, packageTgz, repository.getSubLayout());
        if (NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout())) {
            OhpmPublishRes res = OhpmPublishRes.builder()
                    .additionalMsg("")
                    .success(true)
                    .build();
            return ResponseEntity.ok(res);
        }
        return ResponseEntity.ok("");
    }

    /**
     * Resolves if a passed name contains '/-rev' substring.
     * Npm 'unpublish' a single version package comprises 4 requests: GET, PUT, GET, DELETE, and
     * PUT method has a path that Folib maps on
     * {@link NpmArtifactController#publish(Repository, String, HttpServletRequest)}.
     * Example of PUT path: http://localhost:8080/@scope/package/-rev/0-0000000000000000.
     * As publishing doesn't play any role in 'unpublish' process, it should be skipped.
     *
     * @param name name from path "{storageId}/{repositoryId}/{name:.+}"
     * @return true if contains, false if not. If true PUT stage of 'unpublish' will be skipped.
     */
    private boolean nameContainsRevision(String name) {
        if (name.contains("/-rev/")) {
            logger.warn("Url comprises '/-rev/' sub path");

            return true;
        }
        return false;
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @PutMapping(path = "{storageId}/{repositoryId}/-/user/org.couchdb.user:{username}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addUser(Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new InsufficientAuthenticationException("unauthorized");
        }

        if (!(authentication instanceof UsernamePasswordAuthenticationToken)) {
            return toResponseEntityError("Unsupported authentication class " + authentication.getClass().getName());
        }

        Object principal = authentication.getPrincipal();
        if (!(principal instanceof SpringSecurityUser)) {
            return toResponseEntityError(
                    "Unsupported authentication principal " + Optional.ofNullable(principal).orElse(null));
        }

        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body("{\"ok\":\"user '" + authentication.getName() + "' created\"}");
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageScope}/{packageName}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishPackageWithScope(@RepoMapping Repository repository,
                                                    @PathVariable(name = "packageScope") String packageScope,
                                                    @PathVariable(name = "packageName") String packageName,
                                                    @PathVariable(name = "rev") String rev) {

        logger.info("Npm unpublish a package request: storageId-[{}]; repositoryId-[{}]; packageName-[{}]; revision-[{}];",
                repository.getStorage().getId(),
                repository.getId(),
                packageName,
                rev);

        NpmUnpublishService.Result result = npmUnpublishService.unpublishPackage(repository, packageScope, packageName);

        return processUnpublishResult(result);
    }

    /**
     * Unpublish a single version of a specified package. This mapping works for npm versions > 6.5.0.
     *
     * @param repository   repository
     * @param packageScope package scope
     * @param packageName  package name
     * @param tarball      tarball
     * @param rev          revision value
     * @return result via {@link ResponseEntity} with HTTP status
     * @throws Exception
     */
    @DeleteMapping(path = "{storageId}/{repositoryId}/{r1}/{r2}/{r3}/{packageScope}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersionWithScope(@RepoMapping Repository repository,
                                                    @PathVariable(name = "packageScope") String packageScope,
                                                    @PathVariable(name = "packageName") String packageName,
                                                    @PathVariable(name = "tarball") String tarball,
                                                    @PathVariable(name = "rev") String rev) {

        final String version = getPackageVersion(tarball, packageName).replace(".tgz", "");

        logger.info("Npm unpublish a single version request: storageId-[{}]; repositoryId-[{}]; packageName-[{}]; tarball-[{}]; revision-[{}];",
                repository.getStorage().getId(),
                repository.getId(),
                packageName,
                tarball,
                rev);

        NpmUnpublishService.Result result = npmUnpublishService.unpublishSingleVersion(repository,
                packageScope,
                packageName,
                tarball,
                version);
        return processUnpublishResult(result);
    }

    /**
     * Unpublish a single version of a specified package. This mapping works for npm versions <= 6.5.0.
     *
     * @param repository   repository
     * @param packageScope package scope
     * @param packageName  package name
     * @param tarball      tarball
     * @param rev          revision value
     * @return result via {@link ResponseEntity} with HTTP status
     * @throws Exception
     */
    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageScope}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersionWithScopeV5(@RepoMapping Repository repository,
                                                      @PathVariable(name = "packageScope") String packageScope,
                                                      @PathVariable(name = "packageName") String packageName,
                                                      @PathVariable(name = "tarball") String tarball,
                                                      @PathVariable(name = "rev") String rev) {

        final String version = getPackageVersion(tarball, packageName).replace(".tgz", "");

        logger.info("Npm unpublish a single version request: storageId-[{}]; repositoryId-[{}]; packageName-[{}]; tarball-[{}]; revision-[{}];",
                repository.getStorage().getId(),
                repository.getId(),
                packageName,
                tarball,
                rev);

        NpmUnpublishService.Result result = npmUnpublishService.unpublishSingleVersion(repository,
                packageScope,
                packageName,
                tarball,
                version);
        return processUnpublishResult(result);
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageName}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishPackage(@RepoMapping Repository repository,
                                           @PathVariable(name = "packageName") String packageName,
                                           @PathVariable(name = "rev") String rev) {
        return unpublishPackageWithScope(repository, null, packageName, rev);
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{r1}/{r2}/{r3}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersion(@RepoMapping Repository repository,
                                           @PathVariable(name = "packageName") String packageName,
                                           @PathVariable(name = "tarball") String tarball,
                                           @PathVariable(name = "rev") String rev) {
        return unpublishVersionWithScope(repository, null, packageName, tarball, rev);
    }

    @DeleteMapping(path = "{storageId}/{repositoryId}/{packageName}/-/{tarball}/-rev/{rev}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    public ResponseEntity unpublishVersionV5(@RepoMapping Repository repository,
                                             @PathVariable(name = "packageName") String packageName,
                                             @PathVariable(name = "tarball") String tarball,
                                             @PathVariable(name = "rev") String rev) {
        return unpublishVersionWithScopeV5(repository, null, packageName, tarball, rev);
    }

    @ApiOperation(value = "ohpm登录")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Successful operation", response = OhpmLoginRes.class),
            @ApiResponse(code = 401, message = "Unauthorized"),
            @ApiResponse(code = 403, message = "Forbidden"),
    })
    @PostMapping(path = "{storageId}/{repositoryId}/login")
    public ResponseEntity<?> ohpmLogin(@PathVariable(name = "storageId") String storageId,
                                       @PathVariable(name = "repositoryId") String repositoryId,
                                       @RequestBody OhpmLoginReq ohpmLoginReq) {

        if (ohpmLoginReq.getPublishId() != null) {
            JSONObject data = new JSONObject();
            String publishId = ohpmLoginReq.getPublishId();
            byte[] decoded = Base64.getDecoder().decode(publishId);
            String basic = new String(decoded, StandardCharsets.UTF_8);
            String[] accountArr = basic.split(":");
            if (accountArr.length != 2) {
                data.put("success", false);
                data.put("error", "The username or password is null!");
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(data);
            }
            String username = accountArr[0], password = accountArr[1];
            try {
                authenticationManager.authenticate(new PasswordAuthentication(username, password));
                String token = userService.generateSecurityToken(username, 7200);
                OhpmLoginRes ohpmLoginRes = OhpmLoginRes.builder()
                        .success(true)
                        .token("Bearer " + token)
                        .message("")
                        .build();
                return ResponseEntity.ok(ohpmLoginRes);
            } catch (Exception e) {
                logger.error(ExceptionUtils.getStackTrace(e));
                if (e instanceof BadCredentialsException) {
                    data.put("success", false);
                    data.put("error", "The username or password is invalid!");
                    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(data);
                }
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
            }

        }
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    private void storeNpmPackage(Repository repository,
                                 NpmCoordinates coordinates,
                                 PackageVersion packageDef,
                                 Path packageTgzTmp, String npmSubLayout)
            throws IOException,
            ProviderImplementationException,
            NoSuchAlgorithmException,
            ArtifactCoordinatesValidationException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        try (InputStream is = new BufferedInputStream(Files.newInputStream(packageTgzTmp))) {
            artifactManagementService.validateAndStore(repositoryPath, is);
        }
        Path packageJsonTmp = npmComponent.extractPackageJson(packageTgzTmp, npmSubLayout, packageDef);
        String packageName = NpmSubLayout.OHPM.getValue().equals(npmSubLayout) ? "oh-package.json5" : "package.json";
        RepositoryPath packageJsonPath = repositoryPathResolver.resolve(repository,
                repositoryPath.resolveSibling(packageName));
        try (InputStream is = new BufferedInputStream(Files.newInputStream(packageJsonTmp))) {
            artifactManagementService.validateAndStore(packageJsonPath, is);
        }


        String shasum = Optional.ofNullable(packageDef.getDist()).map(p -> p.getShasum()).orElse(null);
        if (shasum == null) {
            logger.warn("No checksum provided for package [{}]", packageDef.getName());
            return;
        }

        String packageFileName = repositoryPath.getFileName().toString();
        RepositoryPath checksumPath = repositoryPath.resolveSibling(packageFileName + ".sha1");
        try ( ByteArrayInputStream is = new ByteArrayInputStream(shasum.getBytes(StandardCharsets.UTF_8))){
            artifactManagementService.validateAndStore(checksumPath,is);
        }


        Files.delete(packageTgzTmp);
        Files.delete(packageJsonTmp);

    }

    private String getPackageVersion(String packageNameWithVersion,
                                     String packageName) {
        return packageNameWithVersion.substring(packageName.length() + 1);
    }

    private String getPackageJsonVersion(String packageJsonNameWithVersion, String subLayout) {
        if (NpmSubLayout.OHPM.getValue().equals(subLayout)) {
            return packageJsonNameWithVersion.substring("oh-package".length() + 1);
        }
        return packageJsonNameWithVersion.substring("package".length() + 1);
    }

    private ResponseEntity processUnpublishResult(NpmUnpublishService.Result result) {
        switch (result) {
            case INTERNAL_SERVER_ERROR:
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
            case ARTIFACT_DOES_NOT_EXIST:
            case UNPUBLISHED:
                return ResponseEntity.status(HttpStatus.OK).build();
            case UNPUBLISH_DISABLED:
                ResponseEntity.status(HttpStatus.BAD_REQUEST)
                        .body("Enable 'unpublish' at first");
            default:
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
        }
    }
}
