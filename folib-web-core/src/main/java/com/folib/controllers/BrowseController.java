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
package com.folib.controllers;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.folib.services.*;
import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.booters.PropertiesBooter;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.scan.ScanComponent;
import com.folib.components.thirdparty.foeyes.enums.UploadStatusEnum;
import com.folib.dependency.snippet.CodeSnippet;
import com.folib.dependency.snippet.SnippetGenerator;
import com.folib.domain.Artifact;
import com.folib.domain.DirectoryListing;
import com.folib.domain.FileContent;
import com.folib.domain.bom.Bom;
import com.folib.domain.bom.FoEyes;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.schema2.Manifests;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.domain.Privileges;
import com.folib.utils.DockerUtils;
import com.folib.aql.utils.TreeUtil;
import com.folib.web.RepoMapping;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Resource;
import javax.inject.Inject;
import javax.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

/**
 * REST API for browsing storage/repository/filesystem structures.
 *
 * @author Guido Grazioli <guido.grazioli@gmail.com>
 */
@Slf4j
@RestController
@RequestMapping(path = BrowseController.ROOT_CONTEXT)
@Api(description = "浏览存储/存储库/文件系统结构 控制器", tags = "浏览存储/存储库/文件系统结构 控制器")
public class BrowseController
        extends BaseArtifactController {

    private static final Logger logger = LoggerFactory.getLogger(BrowseController.class);

    // must be the same as @RequestMapping value on the class definition
    public final static String ROOT_CONTEXT = "/api/browse";
    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private SnippetGenerator snippetGenerator;

    @Inject
    private PropertiesBooter propertiesBooter;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @Inject
    @Lazy
    private ArtifactService artifactService;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private ScanComponent scanComponent;

    @Resource
    private ArtifactMetadataService artifactMetadataService;

    @Inject
    @Lazy
    private ArtifactWebService artifactWebService;

    @Inject
    @Lazy
    private RestoreArtifactService restoreArtifactService;

    @Inject()
    @Named("asyncApiBrowseThreadPoolExecutor")
    private ThreadPoolTaskExecutor asyncApiBrowseThreadPoolExecutor;

    @Value("${folib.dockerBrowseCompatibility:false}")
    private boolean dockerBrowseCompatibility;

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/getArtifact/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity getArtifact(@PathVariable String artifactPath,
                                      @PathVariable String storageId,
                                      @PathVariable String repositoryId,
                                      @RequestParam(value = "type", required = false) String type,
                                      @RequestParam(value = "digest", required = false) String digest,
                                      @RequestParam(value = "report", required = false) Boolean report, @RepoMapping Repository repositoryParam) throws JsonProcessingException {
        Map<String, Object> jsonObject = new HashMap<>();
        if (StringUtils.isBlank(type)) {
            type = repositoryParam.getLayout();
        }
        if (!DockerLayoutProvider.ALIAS.equalsIgnoreCase(type)) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            Repository repository = repositoryPath.getRepository();
            Artifact artifact = getArtifact(repositoryPath, report);
            if (artifact != null) {
                List<CodeSnippet> snippets = snippetGenerator.generateSnippets(repository.getLayout(), artifact.getArtifactCoordinates());
                jsonObject.put("snippets", snippets);
            }

            if (artifact != null) {
                TreeUtil treeUtil = new TreeUtil();
                SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                if (artifact.getCreated() != null) {
                    String createdTime = DateUtil.format(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                    jsonObject.put("createdTime", createdTime);
                }

                if (artifact.getLastUsed() != null) {
                    String lastUsedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                    jsonObject.put("lastUsedTime", lastUsedTime);
                }

                if (artifact.getLastUpdated() != null) {
                    String lastModified = DateUtil.format(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                    jsonObject.put("lastModified", lastModified);
                }

                if (artifact.getScanDateTime() != null) {
                    String scanDateTime = DateUtil.format(Date.from(artifact.getScanDateTime().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                    jsonObject.put("scanTime", scanDateTime);
                }

                Set<String> fileNames = artifact.getArtifactArchiveListing().getFilenames();

                if (fileNames != null && fileNames.size() > 0) {
                    List listTree = treeUtil.toTree(fileNames);
                    jsonObject.put("listTree", listTree);
                }

                jsonObject.put("downloadCount", artifact.getDownloadCount());
                jsonObject.put("sha", artifact.getChecksums().get("SHA-1"));
                jsonObject.put("md5", artifact.getChecksums().get("MD5"));
                jsonObject.put("artifact", artifact);
                getBom(jsonObject, repositoryPath);
            }
        } else {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            try {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.include(file.getName())).collect(Collectors.toList());
                FileContent fileContent = fileContents.get(0);
                RepositoryPath versionPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath + File.separator + fileContent.getName());
                Artifact artifact = getArtifact(repositoryPathResolver.resolve(storageId, repositoryId, fileContent.getArtifactPath()), report);
                DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
                jsonObject.put("artifact", artifact);
                String manifestString = Files.readString(versionPath);

                String imageName = getBaseUrlSimple(storageId, repositoryId) + "/" + dockerArtifactCoordinates.getIMAGE_NAME();
                String code = "docker  pull  " + imageName;
                CodeSnippet codeSnippet = new CodeSnippet("Docker", code);
                List<CodeSnippet> snippets = new ArrayList<>();
                snippets.add(codeSnippet);
                ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                String configDigest = "";
                if (Objects.nonNull(imageManifest.getConfig())) {
                    configDigest = imageManifest.getConfig().getDigest();
                } else if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
                    Manifests manifests = imageManifest.getManifests().get(0);
                    if (StringUtils.isNotBlank(digest)) {
                        Optional<Manifests> optionalManifests = imageManifest.getManifests().stream().filter(item -> item.getDigest().equals(digest)).findFirst();
                        if (optionalManifests.isPresent()) {
                            manifests = optionalManifests.get();
                        }
                    }
                    RepositoryPath manifestPath = repositoryPathResolver.resolve(storageId, repositoryId, "manifest/" + manifests.getDigest());
                    if (Objects.nonNull(manifestPath) && Files.exists(manifestPath)) {
                        ImageManifest manifest = JSON.parseObject(Files.readString(manifestPath), ImageManifest.class);
                        if (Objects.nonNull(manifest)) {
                            List<Manifests> manifestsList = imageManifest.getManifests();
                            imageManifest = manifest;
                            imageManifest.setManifests(manifestsList);
                            configDigest = manifest.getConfig().getDigest();
                        }
                    }
                }
                if (StringUtils.isNotBlank(configDigest)) {
                    RepositoryPath manifestConfigPath = repositoryPathResolver.resolve(storageId, repositoryId, "blobs/" + configDigest);
                    if (Files.exists(manifestConfigPath)) {
                        String manifestConfigString = Files.readString(manifestConfigPath);
                        JSONObject object = JSON.parseObject(manifestConfigString);
                        jsonObject.put("manifestConfig", object);
                    }
                }
                jsonObject.put("sha256", artifact.getArtifactName());
                Long size = Optional.ofNullable(imageManifest.getLayers()).orElse(Collections.emptyList()).stream().filter(item -> Objects.nonNull(item.getSize())).mapToLong(LayerManifest::getSize).sum();
                jsonObject.put("snippets", snippets);
                jsonObject.put("manifest", imageManifest);
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                jsonObject.put("lastModified", DateUtil.format(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), dateFormat));
                if (artifact.getScanDateTime() != null) {
                    String scanDateTime = DateUtil.format(Date.from(artifact.getScanDateTime().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), dateFormat);
                    jsonObject.put("scanTime", scanDateTime);
                }
                jsonObject.put("size", size);
                jsonObject.put("imageName", imageName);
                jsonObject.put("subsidiaryFiles", DockerUtils.getDockerSubsidiaryFilePaths(repositoryPath));
                getBom(jsonObject, versionPath);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return ResponseEntity.status(OK)
                .body(jsonObject);
    }


    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @ApiOperation(value = "List the contents for a docker.")
    @GetMapping(value = "/getDockerArtifact/{storageId}/{repositoryId}/{path:.+}")
    public Object getDockerArtifact(@PathVariable("storageId") String storageId,
                                    @PathVariable("repositoryId") String repositoryId,
                                    @PathVariable(value = "path", required = false) String path) {
        JSONObject jsonObject = new JSONObject();
        if (path == null) {

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            try {
                Repository repository = repositoryPath.getRepository();
                DirectoryListing directoryListing;
                if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    directoryListing = directoryListingService.fromGroupRepositoryPath(repository, repositoryPath);
                } else {
                    directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                }
                List<FileContent> imageDirList = directoryListing.getDirectories();
                imageDirList = imageDirList.stream().filter(f -> !DockerLayoutProvider.BLOBS.equalsIgnoreCase(f.getName()) && !DockerLayoutProvider.MANIFEST.equalsIgnoreCase(f.getName())).collect(Collectors.toList());
                jsonObject.put("directories", imageDirList);
                jsonObject.put("files", new JSONArray());
            } catch (IOException e) {
                jsonObject.put("directories", new JSONArray());
                jsonObject.put("message", "获取失败");
            }
            return ResponseEntity.status(OK)
                    .body(jsonObject);
        } else {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            try {
                Repository repository = repositoryPath.getRepository();
                DirectoryListing directoryListing;
                if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    directoryListing = directoryListingService.fromGroupRepositoryPath(repository, repositoryPath);
                } else {
                    directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                }
                List<FileContent> imageDirList = Collections.synchronizedList(Lists.newArrayList()), directories = Collections.synchronizedList(Lists.newArrayList());
                if (dockerBrowseCompatibility) {
                    CompletableFuture<Void> allOf = CompletableFuture.allOf(directoryListing.getDirectories().stream().map(f -> CompletableFuture.runAsync(() -> {
                        try {
                            String uuidPrefix = f.getStorageId() + "-" + f.getRepositoryId() + "-" + f.getArtifactPath() + "/sha256";
                            long count = artifactWebService.countByUUidPrefix(uuidPrefix);
                            if (count > 0L) {
                                imageDirList.add(f);
                            } else {
                                directories.add(f);
                            }
                        } catch (Exception ex) {
                            logger.warn(ExceptionUtils.getStackTrace(ex));
                        }
                    }, asyncApiBrowseThreadPoolExecutor)).toArray(CompletableFuture[]::new));
                    try {
                        long startTime = System.currentTimeMillis();
                        allOf.get(30, TimeUnit.SECONDS);
                        imageDirList.sort(Comparator.comparing(FileContent::getArtifactPath));
                        directories.sort(Comparator.comparing(FileContent::getArtifactPath));
                        logger.info("Docker browse finished take time [{}] ms", System.currentTimeMillis() - startTime);
                    } catch (Exception ex) {
                        logger.warn("Error occurred while waiting for tasks to complete [{}]", ExceptionUtils.getStackTrace(ex));
                    }
                } else {
                    directoryListing.getDirectories().forEach(f -> {
                        try (Stream<Path> pathStream = Files.list(repositoryPathResolver.resolve(f.getStorageId(), f.getRepositoryId(), f.getArtifactPath()))) {
                            if (pathStream.anyMatch(DockerCoordinates::isManifestPath)) {
                                imageDirList.add(f);
                            } else if (!DockerLayoutProvider.BLOBS.equalsIgnoreCase(f.getName()) && !DockerLayoutProvider.MANIFEST.equalsIgnoreCase(f.getName())) {
                                directories.add(f);
                            }
                        } catch (Exception ex) {
                            logger.warn(ExceptionUtils.getStackTrace(ex));
                        }
                    });
                }
                jsonObject.put("files", imageDirList);
                jsonObject.put("directories", directories);
            } catch (IOException e) {
                jsonObject.put("files", new JSONArray());
                jsonObject.put("message", "获取失败");
            }

        }
        return jsonObject;
    }

    @ApiOperation(value = "List configured storages.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The list was returned."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(produces = {MediaType.TEXT_PLAIN_VALUE,
            MediaType.TEXT_HTML_VALUE,
            MediaType.APPLICATION_JSON_VALUE})
    public Object storages(ModelMap model,
                           HttpServletRequest request,
                           @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader) {
        logger.info("Requested browsing for storages");

        try {
            Map<String, Storage> storages = configurationManager.getConfiguration().getStorages();
            DirectoryListing directoryListing = directoryListingService.fromStorages(storages);

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }

            model.addAttribute("showBack", false);
            model.addAttribute("currentUrl", StringUtils.chomp(request.getRequestURI(), "/"));
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        } catch (Exception e) {
            String message = "Attempt to browse storages failed. Check server logs for more information.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    @ApiOperation(value = "List configured repositories for a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The list was returned."),
            @ApiResponse(code = 404, message = "The requested storage was not found."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/{storageId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.TEXT_HTML_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public Object repositories(@ApiParam(value = "The storageId", required = true) @PathVariable("storageId") String storageId,
                               HttpServletRequest request,
                               ModelMap model,
                               @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader) {
        logger.info("Requested browsing for repositories in storage : {}", storageId);

        try {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            if (storage == null) {
                return getNotFoundResponseEntity("The requested storage was not found.", acceptHeader);
            }

            DirectoryListing directoryListing = directoryListingService.fromRepositories(storage.getRepositories());

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }

            model.addAttribute("currentUrl", StringUtils.chomp(request.getRequestURI(), "/"));
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        } catch (Exception e) {
            String message = "Attempt to browse repositories failed. Check server logs for more information.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    @ApiOperation(value = "Deletes a path from a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deleted."),
            @ApiResponse(code = 400, message = "Bad request."),
            @ApiResponse(code = 404, message = "The specified storageId/repositoryId/path does not exist!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity delete(@RepoMapping Repository repository,
                                 @ApiParam(value = "Whether to use force delete")
                                 @RequestParam(defaultValue = "false",
                                         name = "force",
                                         required = false) boolean force,
                                 @PathVariable String artifactPath)
            throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Deleting {}:{}/{}...", storageId, repositoryId, artifactPath);

        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath)) {
                return ResponseEntity.status(NOT_FOUND)
                        .body("The specified path does not exist!");
            }

            artifactManagementService.delete(repositoryPath, force);
        } catch (ArtifactStorageException e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(e.getMessage());
        }

        return ResponseEntity.ok("The artifact was deleted.");
    }


    @ApiOperation(value = "List the contents for a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The list was returned."),
            @ApiResponse(code = 404, message = "The requested storage, repository, or path was not found."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = {"{storageId}/{repositoryId}/{path:.+}"},
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.TEXT_HTML_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public Object repositoryContent(@RepoMapping Repository repository,
                                    @PathVariable("path") String rawPath,
                                    HttpServletRequest request,
                                    HttpServletResponse response,
                                    @RequestHeader HttpHeaders httpHeaders,
                                    ModelMap model,
                                    @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested browsing repository content at {}/{}/{} ", storageId, repositoryId, rawPath);
        try {
            if (DockerLayoutProvider.ALIAS.equals(repository.getLayout()) && acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return getDockerArtifact(storageId, repositoryId, rawPath);
            }
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, rawPath);
            if (RepositoryFiles.isArtifact(repositoryPath) && Files.exists(repositoryPath)) {
                if (!authComponent.validatePrivileges(repository,  repositoryPath, Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
                    response.setStatus(HttpStatus.FORBIDDEN.value());
                    return null;
                }
                vulnerabilityBlock(repositoryPath);
                provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
                return null;
            }
            DirectoryListing directoryListing = null;
            if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                directoryListing = directoryListingService.fromGroupRepositoryPath(repository, repositoryPath);
            } else {
                if (repositoryPath == null || !Files.exists(repositoryPath)) {
                    return getNotFoundResponseEntity("The requested repository path was not found.", acceptHeader);
                }

                if (!repository.isInService()) {
                    return getServiceUnavailableResponseEntity("Repository is not in service...", acceptHeader);
                }
                if (!repository.isAllowsDirectoryBrowsing() || !probeForDirectoryListing(repositoryPath)) {
                    return getNotFoundResponseEntity("Requested repository doesn't allow browsing.", acceptHeader);
                }
                directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            }
            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }
            String currentUrl = StringUtils.chomp(request.getRequestURI(), "/");
            model.addAttribute("currentUrl", currentUrl);
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        } catch (Exception e) {
            String message = "Failed to generate repository directory listing.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    protected boolean probeForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        return Files.exists(repositoryPath) &&
                repositoryPath.getRepository().getLayout().equals("helm") && repositoryPath.getTarget().toString().endsWith("index.yaml") || Files.isDirectory(repositoryPath) &&
                isPermittedForDirectoryListing(repositoryPath);
    }

    protected boolean isPermittedForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        //TODO: RepositoryFiles.isIndex(repositoryPath) || (
        return (!Files.isHidden(repositoryPath)
                // 支持Cocoapods索引目录的显示
                || repositoryPath.toString().contains(".specs"))
                && !RepositoryFiles.isTemp(repositoryPath);
    }

    private Artifact getArtifact(RepositoryPath repositoryPath, Boolean report) {
        try {
            Artifact artifact = artifactService.findArtifact(repositoryPath, report);
            try {
                if (Objects.nonNull(artifact) && Boolean.TRUE.equals(report)) {
                    String defaultContent = "[]", reportContent = artifact.getReport();
                    if (StringUtils.isNotBlank(reportContent) && !defaultContent.equals(reportContent)) {
                        //图库report字段存在值，移除图库report字段的值，写入到文件中
                        scanComponent.writeReport(repositoryPath, reportContent);
                        resetReport(artifact, defaultContent);
                    } else {
                        //图库report字段不存在值，读取扫描报告文件
                        reportContent = scanComponent.readReport(repositoryPath);
                    }
                    artifact.setReport(reportContent);
                }
            } catch (Exception ex) {
                logger.warn("Handle artifact [{}] report error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
            }
            return artifact;
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private void resetReport(Artifact artifact, String defaultContent) {
        try {
            artifact.setReport(defaultContent);
            artifactService.saveOrUpdateArtifact(artifact);
        } catch (Exception ex) {
            logger.warn("Reset artifact [{}] report error [{}]", artifact.getUuid(), ExceptionUtils.getStackTrace(ex));
        }
    }

    private void getBom(Map<String,Object> data, RepositoryPath repositoryPath) {
        try {
            data.put("bom", false);
            RepositoryPath bomRepositoryPath = artifactComponent.getBomRepositoryPath(repositoryPath);
            if (!Files.exists(bomRepositoryPath)) {
                return;
            }
            Bom bom = JSONObject.parseObject(Files.readString(bomRepositoryPath), Bom.class);
            if (Objects.isNull(bom)) {
                return;
            }
            FoEyes foEyes = bom.getFoEyes();
            if (Objects.isNull(foEyes)) {
                return;
            }
            if (!UploadStatusEnum.UPLOAD_SUCCESS.getType().equals(foEyes.getUploadStatus())) {
                return;
            }
            data.put("bom", true);
        } catch (Exception ex) {
            logger.error("Get repositoryPath [{}] bom info error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
    }

}
