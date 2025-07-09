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
package com.folib.controllers.layout.huggingface;

import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.folib.model.request.*;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.folib.controllers.BaseArtifactController;
import com.folib.domain.Artifact;
import com.folib.domain.gitls.model.GitLfsBatchJson;
import com.folib.domain.gitls.model.GitLfsJson;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.constant.MlModelSystemProperties;
import com.folib.index.MlModelIndexUtils;
import com.folib.model.CardData;
import com.folib.model.RevisionData;
import com.folib.model.SiblingItem;
import com.folib.utils.MlModelRemoteUtils;
import com.folib.utils.MlModelUtils;
import com.folib.utils.PathUtils;
import com.folib.providers.ProviderImplementationException;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.repositories.ArtifactRepository;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.servlet.http.HttpServletResponse;
import lombok.NonNull;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;


import javax.annotation.Nullable;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.Semaphore;
import java.util.stream.Collectors;

import static com.folib.constant.MlModelConstants.LEAD_FILE_NAME;


@RestController
@LayoutReqMapping(HuggingFaceLayoutProvider.ALIAS)
@Api(value = "Hugging Face ML 坐标控制器", tags = "Hugging Face ML 坐标控制器")
public class HuggingFaceMLControllers extends BaseArtifactController {

    private static final Logger log = LoggerFactory.getLogger(HuggingFaceMLControllers.class);

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private HuggingFaceLayoutProvider layoutProvider;

    private static final String EMPTY_REVISION = "tmp";

    //用于判断是否使用lfs上传 1073741824 Bytes 1G
    private final long lfsFileMinSize = 209715200;

    private static final String HF_DEFAULT_REVISION = "main";

    private static final String TEMP_UPLOAD_DIR = "hfml_upload";

    private static final JsonFactory JSON_FACTORY = new JsonFactory();

    //并发控制
    private static final int MAX_CONCURRENT_THREADS = 5;

    private final Semaphore concurrentCommitExecutionsLimit = new Semaphore(MAX_CONCURRENT_THREADS);


    @ApiOperation(value = "从具有组织名称的特定修订中获取文件头响应", nickname = "getFileHeaderWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{organization}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.HEAD
    )
    public ResponseEntity<?> getHead(@RepoMapping Repository repository,
                                     HttpServletRequest request,
                                     @PathVariable("organization") String organizationName,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(revision)
                .originalRemoteCommit(revision)
                .file(filename)
                .request(request).build();

        String remoteBaseUrl = getRemoteUrl(repository);
        RevisionData revisionData = fetchRevision(context, remoteBaseUrl);
        //RevisionData revisionData = fetchRevisionData( context,  repository);
        return fetch(context, revisionData, remoteBaseUrl);
    }


    @ApiOperation(value = "从没有组织名称的特定修订中获取文件头响应", nickname = "getFileHeaderWithoutOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.HEAD
    )
    public ResponseEntity<?> getHead(@RepoMapping Repository repository,
                                     HttpServletRequest request,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .file(filename)
                .request(request)
                .build();
        String remoteBaseUrl = getRemoteUrl(repository);
        RevisionData revisionData = fetchRevision(context, remoteBaseUrl);
        return fetch(context, revisionData, remoteBaseUrl);
        //return strategyFactory.getStrategy(repository.getType()).fetchHeaders(context, response);
    }


    @ApiOperation(value = "从具有组织名称的特定修订版中获取文件", nickname = "getFileWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{organization}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getFile(@RepoMapping Repository repository,
                                     HttpServletRequest request,
                                     @PathVariable("organization") String organizationName,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName).modelName(modelName)
                .revision(revision)
                .file(filename)
                .request(request)
                .build();
        String remoteBaseUrl = getRemoteUrl(repository);
        RevisionData revisionData = fetchRevision(context, remoteBaseUrl);
        return fetch(context, revisionData, remoteBaseUrl);
        // return strategyFactory.getStrategy(repository.getType()).fetchFile(context);

    }


    @ApiOperation(value = "从没有组织名称的特定修订版本中获取文件", nickname = "getFileWithoutOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{modelName}/resolve/{revision}/{filename:.+}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getFile(@RepoMapping Repository repository,
                                     HttpServletRequest request,
                                     @PathVariable("modelName") String modelName,
                                     @PathVariable("revision") String revision,
                                     @PathVariable("filename") String filename) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .file(filename)
                .request(request)
                .build();
        String remoteBaseUrl = getRemoteUrl(repository);
        RevisionData revisionData = fetchRevision(context, remoteBaseUrl);
        return fetch(context, revisionData, remoteBaseUrl);
        //return strategyFactory.getStrategy(repository.getType()).fetchFile(context);

    }

    @ApiOperation(value = "获取带有组织参数的主要修订信息", nickname = "getMainRevisionInfoWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getMainRevisionData(@RepoMapping Repository repository,
                                                 HttpServletRequest request,
                                                 @PathVariable("organization") String organizationName,
                                                 @PathVariable("modelName") String modelName) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(HF_DEFAULT_REVISION)
                .request(request).build();
        String remoteBaseUrl = getRemoteUrl(repository);
        return ResponseEntity.ok(fetchRevision(context, remoteBaseUrl));
        // return ResponseEntity.ok(strategyFactory.getStrategy(repository.getType()).fetchRevisionData(context));
    }

    @ApiOperation(value = "获取没有组织参数的主要修订信息", nickname = "getMainRevisionInfoWithoutOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}",
            method = RequestMethod.GET
    )
    public ResponseEntity<?> getMainRevisionData(@RepoMapping Repository repository,
                                                 HttpServletRequest request,
                                                 @PathVariable("modelName") String modelName) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(HF_DEFAULT_REVISION)
                .request(request)
                .build();
        String remoteBaseUrl = getRemoteUrl(repository);
        return ResponseEntity.ok(fetchRevision(context, remoteBaseUrl));
        //return ResponseEntity.ok(strategyFactory.getStrategy(repository.getType()).fetchRevisionData(context));
    }


    @ApiOperation(value = "获取组织参数的修订信息", nickname = "getRevisionInfoWithOrganizationParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}/revision/{revision}",
            method = RequestMethod.GET
    )
    public void getRevisionData(@RepoMapping Repository repository,
                                @PathVariable("storageId") String storageId,
                                @PathVariable("repositoryId") String repositoryId,
                                @PathVariable("organization") String organizationName,
                                @PathVariable("modelName") String modelName,
                                @PathVariable("revision") String revision,
                                HttpServletRequest request,
                                HttpServletResponse response,
                                @RequestHeader HttpHeaders headers) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();
        String remoteBaseUrl = getRemoteUrl(repository);
        String alternativeUrl = remoteBaseUrl == null ? null : MlModelRemoteUtils.getModelInfoAlternativeUrl(context.getOrg(), context.getModelName(), context.getRevision(), remoteBaseUrl);
        String modelInfoPath = MlModelUtils.getLatestModelInfoPath(context);
        RepositoryPath tagPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), modelInfoPath);
        tagPath.setTargetUrl(alternativeUrl);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, headers, repositoryPath);
        //return ResponseEntity.ok(strategyFactory.getStrategy(repository.getType()).fetchRevisionData(context));

    }

    @ApiOperation(value = "获取没有组织参数的修订信息", nickname = "getRevisionInfoWithoutOrganizationParam")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}/revision/{revision}",
            method = RequestMethod.GET
    )
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public void getRevisionData(@RepoMapping Repository repository,
                                HttpServletRequest request,
                                HttpServletResponse response,
                                @RequestHeader HttpHeaders headers,
                                @PathVariable("modelName") String modelName,
                                @PathVariable("revision") String revision) throws Exception {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();
        String remoteBaseUrl = getRemoteUrl(repository);
        String alternativeUrl = repository.getRemoteRepository() == null ? null : MlModelRemoteUtils.getModelInfoAlternativeUrl(context.getOrg(), context.getModelName(), context.getRevision(), remoteBaseUrl);
        String modelInfoPath = MlModelUtils.getLatestModelInfoPath(context);
        RepositoryPath tagPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), modelInfoPath);
        tagPath.setTargetUrl(alternativeUrl);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, headers, repositoryPath);
        //return ResponseEntity.ok(strategyFactory.getStrategy(repository.getType()).fetchRevisionData(context));

    }

    @ApiOperation(value = "上传组织名称参数的 LFS", nickname = "UploadLFSWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/complete_multipart/{organization}/{modelName}/{oid}",
            method = RequestMethod.PUT
    )
    public ResponseEntity<?> uploadMultipart(@RepoMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("organization") String organization,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("oid") String oid, InputStream inputStream) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organization)
                .modelName(modelName)
                .file(oid)
                .request(request)
                .revision(EMPTY_REVISION)
                .build();

        return uploadLfsFile(context, inputStream);

    }

    @ApiOperation(value = "上传没有组织名称参数的 LFS", nickname = "UploadLFSWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/complete_multipart/{modelName}/{oid}",
            method = RequestMethod.PUT
    )
    public ResponseEntity<?> uploadMultipart(@RepoMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("oid") String oid) throws IOException {
        String repositoryId = repository.getId();
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repositoryId)
                .modelName(modelName)
                .file(oid)
                .request(request)
                .revision(EMPTY_REVISION).build();

        return uploadLfsFile(context, request.getInputStream());
    }


    @ApiOperation(value = "处理有组织名称的 LFS 对象", nickname = "PreUploadLFSWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{organization}/{modelName}.git/info/lfs/objects/batch",
            method = RequestMethod.POST,
            consumes = {"application/vnd.git-lfs+json"},
            produces = {"application/vnd.git-lfs+json"}
    )
    public ResponseEntity<?> handleLfsObjects(@RepoMapping Repository repository,
                                              HttpServletRequest request,
                                              @PathVariable("organization") String organization,
                                              @PathVariable("modelName") String modelName,
                                              @RequestBody GitLfsBatchJson lfsInfoPayload) throws IOException {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organization)
                .modelName(modelName)
                .request(request)
                .build();
        return ResponseEntity.ok(preUploadBatch(context, lfsInfoPayload));

    }


    @ApiOperation(value = "处理没有组织名称的 LFS 对象", nickname = "PreUploadLFSWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/{modelName}.git/info/lfs/objects/batch",
            method = RequestMethod.POST,
            consumes = {"application/vnd.git-lfs+json"},
            produces = {"application/vnd.git-lfs+json"}
    )
    public ResponseEntity<?> handleLfsObjects(@RepoMapping Repository repository,
                                              HttpServletRequest request,
                                              @PathVariable("modelName") String modelName,
                                              @RequestBody GitLfsBatchJson lfsInfoPayload) throws IOException {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .request(request)
                .build();

        return ResponseEntity.ok(preUploadBatch(context, lfsInfoPayload));

    }


    @ApiOperation(value = "获取 preUpload 响应，以管理具有组织名称的每个文件的上传类型", nickname = "PreUploadModelWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}/preupload/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/json"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handlePreUpload(@RepoMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("organization") String organizationName,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("revision") String revision,
                                             @RequestBody MlFilesRequest filesRequest) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organizationName)
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .file("")
                .build();

        // MlFilesResponse mlFilesResponse = strategyFactory.getStrategy(repository.getType()).handlePreUpload(context, filesRequest);
        return ResponseEntity.ok(preUploadDir(context, filesRequest));

    }


    @ApiOperation(value = "获取 preUpload 响应，以管理每个文件的上传类型，而无需组织名称", nickname = "PreUploadModelWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}/preupload/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/json"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handlePreUpload(@RepoMapping Repository repository,
                                             HttpServletRequest request,
                                             @PathVariable("modelName") String modelName,
                                             @PathVariable("revision") String revision,
                                             @RequestBody MlFilesRequest filesRequest) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .file("")
                .build();

        //MlFilesResponse mlFilesResponse = strategyFactory.getStrategy(repository.getType()).handlePreUpload(context, filesRequest);
        return ResponseEntity.ok(preUploadDir(context, filesRequest));
    }


    @ApiOperation(value = "将模型上传为具有组织名称的新修订版本", nickname = "UploadModelWithOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{organization}/{modelName}/commit/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/x-ndjson"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handleCommit(@RepoMapping Repository repository,
                                          HttpServletRequest request,
                                          @PathVariable("organization") String organization,
                                          @PathVariable("modelName") String modelName,
                                          @PathVariable("revision") String revision,
                                          InputStream bodyStream) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .org(organization)
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();
        if (!this.concurrentCommitExecutionsLimit.tryAcquire()) {
            log.info("Upload for organization {}, modelName {} and revision {} is cancelled. Concurrent uploads limit {} reached.", organization, modelName, revision, MlModelSystemProperties.ML_MODEL_CONCURRENT_UPLOADS.name());
            return ResponseEntity.status(HttpStatus.TOO_MANY_REQUESTS).build();
        }
        try {
            log.debug("Handle commit payload size is {} bytes", request.getHeader("content-length"));
            //String commitMessage = strategyFactory.getStrategy(repository.getType()).handleCommit(context, bodyStream);
            String commitMessage = uploadDir(context, bodyStream);
            return ResponseEntity.ok(new MlCommitInfo("commitUrl", commitMessage, commitMessage, "commitOid", null));
        } finally {
            this.concurrentCommitExecutionsLimit.release();
        }


    }


    @ApiOperation(value = "将模型作为新修订版本上传，不带组织名称", nickname = "UploadModelWithoutOrganizationNameParam")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/models/{modelName}/commit/{revision}",
            method = RequestMethod.POST,
            consumes = {"application/x-ndjson"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> handleCommit(@RepoMapping Repository repository,
                                          HttpServletRequest request,
                                          @PathVariable("modelName") String modelName,
                                          @PathVariable("revision") String revision,
                                          InputStream body) {
        MlModelRequestContext context = MlModelRequestContext.builder()
                .storageId(repository.getStorage().getId())
                .repositoryId(repository.getId())
                .modelName(modelName)
                .revision(revision)
                .request(request)
                .build();

        //String commitMessage = strategyFactory.getStrategy(repository.getType()).handleCommit(context, body);
        String commitMessage = uploadDir(context, body);
        return ResponseEntity.ok(new MlCommitInfo("commitUrl", commitMessage, commitMessage, "commitOid", null));

    }

    @ApiOperation(value = "自动通过元数据验证", nickname = "AutoPassMetadataValidation")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/validate-yaml",
            method = RequestMethod.POST,
            consumes = {"application/json"},
            produces = {"application/json"}
    )
    public ResponseEntity<?> passMetadataValidation(InputStream body) {
        return ResponseEntity.ok("{}");
    }


    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/models/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepoMapping Repository repository,
                         @PathVariable String artifactPath,
                         @RequestHeader HttpHeaders httpHeaders, HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String path = String.join("/", "models", artifactPath);
        logger.info("Requested get HuggingFaceML  file {}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Content-Disposition", "attachment;" + repositoryPath.getFileName().toString());
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }


    private ResponseEntity<?> fetch(MlModelRequestContext context, RevisionData modelInfo, String remoteBaseUrl) throws IOException {
        String revision = context.getRevision();
        String alternativeUrl = remoteBaseUrl == null ? null : MlModelRemoteUtils.getHuggingFaceAlternativeFileUrl(context, remoteBaseUrl);
        String revisionLocal = context.getVersionFolder();
        if (revision.equals(modelInfo.getSha()) && StrUtil.isNotBlank(remoteBaseUrl)) {
            revisionLocal = "main";
        }
        String modelInfoPath = MlModelUtils.getFilePath(context.getOrg(), context.getModelName(), revisionLocal, modelInfo.getLastModified(), context.getFile());
        RepositoryPath tagPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), modelInfoPath);
        tagPath.setTargetUrl(alternativeUrl);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
        repositoryPath = repositoryPath == null ? tagPath : repositoryPath;
        try (InputStream in = Files.newInputStream(repositoryPath)) {
            vulnerabilityBlock(repositoryPath);
            LayoutFileSystemProvider provider = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId()).getFileSystem().provider();
            final RepositoryPath sha1Path = provider.getChecksumPath(repositoryPath, MessageDigestAlgorithms.SHA_1);
            HttpHeaders headers = new HttpHeaders();
            if (Files.exists(sha1Path)) {
                String sha1 = Files.readString(sha1Path);
                headers.set("ETag", sha1);
            }
            if (Files.exists(repositoryPath)) {
                long artifactSize = Files.size(repositoryPath);
                headers.set("Content-Length", Long.valueOf(artifactSize).toString());
            }
            headers.set("X-Repo-Commit", revision);
            headers.set("Content-Type", "application/octet-stream");
            InputStreamResource resource = new InputStreamResource(in);
            return ResponseEntity.ok()
                    .headers(headers)
                    .body(resource);
        } catch (IOException e) {
            log.error("Failed to find artifact {} in repo {}", repositoryPath.getPath(), context.getRepositoryId());
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .header("X-Error-Code", "EntryNotFound")
                    .header("X-Error-Message", "EntryNotFound").build();
        }

    }


    public RevisionData fetchRevision(MlModelRequestContext requestContext, String remoteBaseUrl) throws Exception {
        RevisionData revisionData;

        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        log.debug("Received fetch revision request for repo {}, organization {}, model {}, revision {}", requestContext
                .getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext
                .getRevision());
        String latestLeadFilePath = getLatestLeadFilePath(requestContext);
        if (latestLeadFilePath == null) {
            return fetchLeadFileByGeneratedSha1Value(requestContext, remoteBaseUrl);
        }
        try {

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(requestContext.getStorageId(), requestContext.getRepositoryId(), latestLeadFilePath);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            try (InputStream leadStream = Files.newInputStream(repositoryPath)) {
                revisionData = MlModelUtils.createObjectMapper().readValue(leadStream, RevisionData.class);
                String mlStr = Objects.isNull(requestContext.getOrg()) ? String.format("models/%s/",requestContext.getModelName()) : String.format("models/%s/%s/", requestContext.getOrg(), requestContext.getModelName());
                String revision = latestLeadFilePath.replace(mlStr,"").replace(String.format("/%s/%s",revisionData.getLastModified(),LEAD_FILE_NAME),"");
                String leadFilePath = MlModelUtils.getFilePath(requestContext.getOrg(), requestContext.getModelName(), requestContext
                        .getRevision(), revisionData.getLastModified(), LEAD_FILE_NAME);

                String revisionFolder = MlModelUtils.getRevisionFolderByTimeStampLeadFilePath(requestContext, leadFilePath, revisionData
                        .getLastModified());
                requestContext.setVersionFolder(revision);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return afterFailedToFetchLatestModelInfo(requestContext, latestLeadFilePath, e, remoteBaseUrl);

        }
        return revisionData;
    }


    private String getLatestLeadFilePath(MlModelRequestContext context) throws IOException {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }

        RepositoryPath tagPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), MlModelUtils.getModelPath(context.getOrg(), context.getModelName()));
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
        repositoryPath = repositoryPath == null ? tagPath : repositoryPath;
        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            return null;
        }
        List<RepositoryPath> fileList = new ArrayList<>();
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    // 在这里可以处理目录（如果需要的话）
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (".folib_huggingface_model_info.json".equals(file.getFileName().toString())) {
                        fileList.add((RepositoryPath) file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }
        Date latestDate = null;
        RepositoryPath latestLeadFile = null;
        for (RepositoryPath leadFile : fileList) {
            String timeStampFolderByLeadFilePath = getTimeStampFolderByLeadFilePath(leadFile.getPath());
            if (StringUtils.isNotBlank(timeStampFolderByLeadFilePath)) {
                try {
                    Date currentDate = MlModelUtils.convertToDate(timeStampFolderByLeadFilePath);
                    if (latestDate == null || latestDate.before(currentDate)) {
                        latestDate = currentDate;
                        latestLeadFile = leadFile;
                    }
                } catch (Exception e) {
                    log.debug("Failed to update latest lead file path with: {}", timeStampFolderByLeadFilePath, e);
                }
            }
        }
        return (latestLeadFile != null) ? latestLeadFile.getPath() : null;
    }

    public static String getTimeStampFolderByLeadFilePath(String leadFilePath) {
        if (leadFilePath == null) {
            throw new NullPointerException("leadFilePath is marked non-null but is null");
        }
        if (!PathUtils.isFolderPath(leadFilePath) &&
                PathUtils.getLastPathElement(leadFilePath).equals(LEAD_FILE_NAME)) {
            String timeStampFolderPath = PathUtils.getParent(leadFilePath);
            return PathUtils.getLastPathElement(timeStampFolderPath);
        }
        return null;
    }

    private RevisionData fetchLeadFileByGeneratedSha1Value(MlModelRequestContext requestContext, String remoteBaseUrl) throws RuntimeException, IOException {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        log.debug("About to try to fetch model info object by search oh generated sha1 property for repoKey:{}, modelId:{}, revision:{}", requestContext
                .getRepositoryId(), requestContext.modelId(), requestContext.getRevision());
        String leadFilePath = getLeadFilePathByGeneratedSha1(requestContext, requestContext.getRevision());

        if (remoteBaseUrl != null) {
            RevisionData revisionData = getRemoteRevisionData(requestContext, remoteBaseUrl);
            if (StringUtils.isNotBlank(revisionData.getLastModified())) {
                leadFilePath = MlModelRemoteUtils.getFilePath(requestContext.getOrg(), requestContext.getModelName(), revisionData.getLastModified(), requestContext.getFile());
                String revisionFolder = String.join("/", "main", revisionData.getLastModified());
                requestContext.setVersionFolder(revisionFolder);
                return revisionData;
            }
            requestContext.setVersionFolder(leadFilePath.replace(LEAD_FILE_NAME, ""));
            log.debug("Found model info object for repoKey:{}, modelId:{}, revision:{} under the path:{}", requestContext.getRepositoryId(), requestContext.modelId(), requestContext.getRevision(), leadFilePath);
            return downloadRevisionData(requestContext, leadFilePath);
        }
        if (leadFilePath != null) {
            RevisionData revisionData = downloadRevisionData(requestContext, leadFilePath);
            if (StringUtils.isNotBlank(revisionData.getLastModified())) {
                String revisionFolder = MlModelUtils.getRevisionFolderByTimeStampLeadFilePath(requestContext, leadFilePath, revisionData
                        .getLastModified());
                requestContext.setVersionFolder(revisionFolder);
                return revisionData;
            }
            requestContext.setVersionFolder(leadFilePath.replace(LEAD_FILE_NAME, ""));
            log.debug("Found model info object for repoKey:{}, modelId:{}, revision:{} under the path:{}", requestContext.getRepositoryId(), requestContext.modelId(), requestContext.getRevision(), leadFilePath);
            return downloadRevisionData(requestContext, leadFilePath);
        }
        log.warn("Could not find model info object for repoKey:{}, modelId:{}, revision:{}", requestContext.getRepositoryId(), requestContext.modelId(), requestContext.getRevision());
        return null;
    }


    private RevisionData afterFailedToFetchLatestModelInfo(MlModelRequestContext context, String path, Exception e, String remoteBaseUrl) throws Exception {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (path == null) {
            throw new NullPointerException("path is marked non-null but is null");
        }
        if (e == null) {
            throw new NullPointerException("e is marked non-null but is null");
        } else {
            log.error("Could not fetch a model info file for repoKey:{}, path:{}, message:{}", context.getRepositoryId(), path, e.getMessage());
            log.debug("Could not fetch a model info file for repoKey:{}, path:{}", context.getRepositoryId(), path, e);
            log.debug("Got 404 status while tried to fetch model info stream for repoKey:{}, path:{} about to try to fetch it by the internal generated revision as:{}", context.getRepositoryId(), path, context.getRevision());
            return fetchLeadFileByGeneratedSha1Value(context, remoteBaseUrl);
        }
    }

    private String getLeadFilePathByGeneratedSha1(MlModelRequestContext context, String generatedSha1) throws IOException {
        String revisionPath = MlModelUtils.getModelRevisionPath(context);
        RepositoryPath tagPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), revisionPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
        repositoryPath = repositoryPath == null ? tagPath : repositoryPath;
        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            return null;
        }
        List<RepositoryPath> fileList = new ArrayList<>();
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    // 在这里可以处理目录（如果需要的话）
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (".folib_huggingface_model_info.json".equals(file.getFileName().toString())) {
                        fileList.add((RepositoryPath) file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }

        return fileList.stream().filter(pa -> LEAD_FILE_NAME.equals(pa.getFileName().toString())).findFirst().map(RepositoryPath::getPath).orElse(null);
    }

    private RevisionData downloadRevisionData(MlModelRequestContext requestContext, String leadFilePath) {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (leadFilePath == null) {
            throw new NullPointerException("leadFilePath is marked non-null but is null");
        }
        try {
            RepositoryPath tagPath = repositoryPathResolver.resolve(requestContext.getStorageId(), requestContext.getRepositoryId(), leadFilePath);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
            repositoryPath = repositoryPath == null ? tagPath : repositoryPath;
            try (InputStream leadStream = Files.newInputStream(repositoryPath);) {
                return MlModelUtils.createObjectMapper().readValue(leadStream, RevisionData.class);
            }
        } catch (IOException e) {
            log.warn("Failed to fetch revision data for for repo {}, organization {}, model {}, revision {}", requestContext
                    .getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision());
            throw new RuntimeException("No artifacts found for revision " + requestContext.getRevision());
        }
    }

    private RevisionData getRemoteRevisionData(MlModelRequestContext context, String remoteBaseUrl) throws IOException {
        String modelInfoPath = MlModelRemoteUtils.getLatestModelInfoPath(context);
        //RepositoryPath path = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), modelInfoPath);

        String alternativeUrl = remoteBaseUrl == null ? null : MlModelRemoteUtils.getHuggingFaceAlternativeFileUrl(context, remoteBaseUrl);
        RepositoryPath tagPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), modelInfoPath);
        tagPath.setTargetUrl(alternativeUrl);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
        repositoryPath = repositoryPath == null ? tagPath : repositoryPath;

        ObjectMapper objectMapper = MlModelUtils.createObjectMapper();
        return objectMapper.readValue(Files.readAllBytes(repositoryPath), RevisionData.class);
    }

    public ResponseEntity<?> uploadLfsFile(MlModelRequestContext context, InputStream stream) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (stream == null) {
            throw new NullPointerException("stream is marked non-null but is null");
        }
        log.debug("Received upload lfs file request for repo/organization/modelName/file '{}'/'{}'/'{}'/'{}'", context
                .getRepositoryId(), context.getOrg(), context.getModelName(), context.getFile());
        try (stream) {
            String uploadPath = MlModelUtils.getLfsTmpUploadPath(context.getOrg(), context.getModelName(), context.getFile());
            Artifact artifact = artifactRepository.findOneArtifact(context.getStorageId(), context.getRepositoryId(), uploadPath);
            if (artifact == null) {
                RepositoryPath tagPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), uploadPath);
                RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
                repositoryPath = repositoryPath == null ? tagPath : repositoryPath;
                if(repositoryPath != null){
                    artifactManagementService.validateAndStore(repositoryPath, stream);
                }else {
                    artifactManagementService.validateAndStore(tagPath, stream);
                }
            }
            return ResponseEntity.ok(uploadPath);
        } catch (IOException | ProviderImplementationException | ArtifactCoordinatesValidationException e) {
            log.error("Failed to upload file '{}'/'{}'/'{}'/'{}'", context.getRepositoryId(), context.getOrg(), context.getModelName(), context.getFile(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }


    public GitLfsBatchJson preUploadBatch(MlModelRequestContext context, GitLfsBatchJson batchLfsJson) throws IOException {
        String storageId = context.getStorageId();
        String repositoryId = context.getRepositoryId();
        String organization = context.getOrg();
        String modelName = context.getModelName();
        HttpServletRequest request = context.getRequest();
        if (repositoryId == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        }
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (batchLfsJson == null) {
            throw new NullPointerException("batchLfsJson is marked non-null but is null");
        }
        if (request == null) {
            throw new NullPointerException("request is marked non-null but is null");
        }
        log.debug("Received batch lfs pre-upload request for repo/organization/modelName '{}'/'{}'/'{}'", repositoryId, organization, modelName);
        List<GitLfsJson> responseJsons = new ArrayList<>();
        String lfsTmpUploadPath = MlModelUtils.getLfsTmpUploadDir(organization, modelName);
        //todo
        //if (!this.securityService.canWrite(repoKey, lfsTmpUploadPath)) {
        //    String errorMessage = "Forbidden: user is missing deploy permission on path: " + lfsTmpUploadPath;
        //    throw new PackageForbiddenException(errorMessage, errorMessage);
        //}

        String baserUrl = configurationManager.getConfiguration().getBaseUrl();
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) + "/storages" : baserUrl + "/storages";
        for (GitLfsJson requestJson : batchLfsJson.getObjects()) {
            boolean sha2ReusePossible = tryToReuseExistingSha2(storageId, repositoryId, organization, modelName, requestJson);
            if (sha2ReusePossible) {
                log.debug("Same sha2 '{}' is already present in the repository '{}'. Upload url will be skipped for organization/modelName '{}'/'{}'.", requestJson.getOid(), repositoryId, organization, modelName);
                responseJsons.add(requestJson);
                continue;
            }
            String uploadPath = MlModelUtils.getLfsUploadEndpoint(storageId, repositoryId, organization, modelName, requestJson.getOid());
            String signedUrl = String.format("%s%s", baserUrl, uploadPath);
            GitLfsJson lfsUploadJson = createLfsUploadJson(requestJson, request.getHeader("Authorization"), storageId, repositoryId, signedUrl);
            lfsUploadJson.setUploadLink(signedUrl);
            responseJsons.add(lfsUploadJson);
        }
        return new GitLfsBatchJson(responseJsons);
    }

    private boolean tryToReuseExistingSha2(String storageId, String repositoryId, String organization, String modelName, GitLfsJson requestJson) throws IOException {
        if (storageId == null) {
            throw new NullPointerException("storageId is marked non-null but is null");
        }
        if (repositoryId == null) {
            throw new NullPointerException("repositoryId is marked non-null but is null");
        }
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }

        String lfsTmpUploadPath = MlModelUtils.getLfsTmpUploadPath(organization, modelName, requestJson.getOid());

        RepositoryPath tagPath = repositoryPathResolver.resolve(storageId, repositoryId, "");
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(tagPath);
        repositoryPath = repositoryPath == null ? tagPath : repositoryPath;
        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            return false;
        }
        List<RepositoryPath> artifactList = new ArrayList<>();
        try {
            //todo 性能优化调整成数据库查询
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (!file.getFileName().toString().startsWith(".")
                            && !file.getFileName().toString().endsWith(".metadata")
                            && !file.getFileName().toString().endsWith(".md5")
                            && !file.getFileName().toString().endsWith(".sha1")
                            && !file.getFileName().toString().endsWith(".sha256")
                            && file.getFileName().toString().length() == 64) {
                        artifactList.add((RepositoryPath) file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }
        RepositoryPath packageArtifact = artifactList.stream().filter(paths -> paths.getFileName().toString().equals(requestJson.getOid())).findFirst().orElse(null);
        boolean found = false;
        if (packageArtifact != null) {
            if (!lfsTmpUploadPath.equals(packageArtifact.getPath())) {
                RepositoryPath tagArtPath = repositoryPathResolver.resolve(storageId, repositoryId, lfsTmpUploadPath);
                RepositoryPath artPath = artifactResolutionService.resolvePath(tagArtPath);
                artPath = artPath == null ? tagArtPath : artPath;
                try (InputStream inputStream = Files.newInputStream(packageArtifact);) {
                    artifactManagementService.store(artPath, inputStream);
                } catch (IOException  e) {
                    throw new RuntimeException(e);
                }
            }
            found = true;
        }
        return Boolean.valueOf(found);
    }

    protected GitLfsJson createLfsUploadJson(GitLfsJson requestJson, String authHeader, String storageId, String repositoryId, String uploadPath) {
        GitLfsJson uploadResponse = new GitLfsJson(requestJson);
        if (isPathExists(storageId, repositoryId, uploadPath)) {
            return uploadResponse;
        }
        String baserUrl = configurationManager.getConfiguration().getBaseUrl();
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) : baserUrl;
        uploadResponse.setUploadLink(GitLfsHelper.getArtifactLfsUrl(baserUrl, storageId, repositoryId, uploadPath, requestJson.getOid()));
        GitLfsHelper.addAuthHeaderIfPresent(uploadResponse.getUploadHeaders(), authHeader);
        GitLfsHelper.addChecksumVerificationHeader(uploadResponse.getUploadHeaders(), requestJson.getOid());
        return uploadResponse;
    }

    protected boolean isPathExists(String storageId, String repositoryId, String oidPath) {
        return this.artifactRepository.artifactExists(storageId, repositoryId, oidPath);
    }

    /**
     * pre 上传目录
     *
     * @param context      上下文对象
     * @param filesRequest 上传文件请求
     */
    public MlFilesResponse preUploadDir(MlModelRequestContext context, MlFilesRequest filesRequest) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (filesRequest == null) {
            throw new NullPointerException("filesRequest is marked non-null but is null");
        }
        if (log.isDebugEnabled()) {
            log.debug("Pre-upload dir request received {}.", context);
        }
        if (filesRequest.getFiles() == null) {
            log.info("Files list is empty for {}.", context);
            throw new RuntimeException("Files list is empty");
        }
        assertValidNames(context);
        if (MlModelUtils.isReleaseRevision(context)) {
            assertModuleAlreadyExist(context);
        }
        List<MlFileInfo> fileInfos = new ArrayList<>();
        for (MlFile file : filesRequest.getFiles()) {
            if (log.isTraceEnabled()) {
                log.trace("File passed for pre-upload has path '{}' and size {} for context {}", file
                        .getPath(), Long.valueOf(file.getSize()), context);
            }
            if (file.getSize() > this.lfsFileMinSize) {
                fileInfos.add(new MlFileInfo(file.getPath(), "lfs", false));
                continue;
            }
            fileInfos.add(new MlFileInfo(file.getPath(), "regular", false));
        }
        return new MlFilesResponse(fileInfos);
    }

    /**
     * 验证名称
     *
     * @param context 上下文对象
     */
    public void assertValidNames(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        boolean isValidName = true;
        String uploadRejectionReason = "";
        String value = "";
        if (!MlModelUtils.isValidRevisionName(context.getRevision())) {
            isValidName = false;
            uploadRejectionReason = "revision name";
            value = context.getRevision();
        } else if (!MlModelUtils.isValidModelName(context.getModelName())) {
            isValidName = false;
            uploadRejectionReason = "model name";
            value = context.getModelName();
        } else if (!MlModelUtils.isValidOrganizationName(context.getOrg())) {
            isValidName = false;
            uploadRejectionReason = "Organization name";
            value = context.getOrg();
        }
        if (!isValidName) {
            String message = String.format("HuggingFace ML module upload rejected, due to invalid %s: %s in repoKey: %s.", uploadRejectionReason, value, context.getRepositoryId());
            log.info(message);
            throw new RuntimeException(message);
        }
    }

    /**
     * 验证模块是否已经存在
     *
     * @param context 上下文对象
     */
    void assertModuleAlreadyExist(MlModelRequestContext context) {
        String repositoryId = context.getRepositoryId();
        String storageId = context.getStorageId();

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, MlModelUtils.getModelRevisionPath(context));
        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            return;
        }
        List<Path> fileList = new ArrayList<>();
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    // 在这里可以处理目录（如果需要的话）
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (!file.getFileName().toString().startsWith(".")
                            && !file.getFileName().toString().endsWith(".metadata")
                            && !file.getFileName().toString().endsWith(".md5")
                            && !file.getFileName().toString().endsWith(".sha1")
                            && !file.getFileName().toString().endsWith(".sha256")) {
                        fileList.add(file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }
        String subRevisionPath = fileList.stream().filter(artifact -> artifact.getFileName().toString().equals(".folib_huggingface_model_info.json")).findFirst().map(artifact -> artifact.getFileName().toString()).orElse(null);
        if (subRevisionPath != null) {
            String message = String.format("HuggingFace ML module conflict. Module: %s already exist in repoKey: %s.", subRevisionPath, repositoryId);
            log.info(message);
            throw new RuntimeException(message);
        }
    }


    public String uploadDir(MlModelRequestContext requestContext, InputStream bodyStream) {
        String commitSummary;
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (bodyStream == null) {
            throw new NullPointerException("bodyStream is marked non-null but is null");
        }
        String repoKey = requestContext.getRepositoryId();
        String organization = requestContext.getOrg();
        String modelName = requestContext.getModelName();
        String revision = requestContext.getRevision();
        String subRevisionFolder = MlModelUtils.formattedDate();
        log.debug("Received commit request for repo {}, organization {}, model {}, revision {}", repoKey, organization, modelName, revision);
        assertValidNames(requestContext);
        assertModuleAlreadyExist(requestContext);
        Set<String> uploadedFiles = new HashSet<>();
        MlKeyValue commitInfo = uploadStream(bodyStream, uploadedFiles, requestContext.getStorageId(), requestContext.getRepositoryId(), organization, modelName, revision, subRevisionFolder);
        processUploadComplete(requestContext, subRevisionFolder);
        if (commitInfo != null && commitInfo.getValue() != null) {
            commitSummary = (String) commitInfo.getValue().get("summary");
        } else {
            log.warn("CommitSummary or its value is null for requestContext {}", requestContext);
            commitSummary = "";
        }
        return commitSummary;
    }

    public List<MlKeyValue> extractParamsFromJson(InputStream jsonInputStream, Path tmpUploadDir) throws IOException {
        if (jsonInputStream == null) {
            throw new NullPointerException("jsonInputStream is marked non-null but is null");
        }
        if (tmpUploadDir == null) {
            throw new NullPointerException("tmpUploadDir is marked non-null but is null");
        }
        List<MlKeyValue> genericFilesData = new ArrayList<>();
        try {
            try (JsonParser jsonParser = JSON_FACTORY.createParser(jsonInputStream)) {
                while (jsonParser.nextToken() != null) {
                    if (jsonParser.getCurrentToken() == JsonToken.START_OBJECT) {
                        genericFilesData.add(parseKeyValue(tmpUploadDir, jsonParser));
                    }
                }
                jsonParser.close();
            }
        } catch (Exception e) {
            for (MlKeyValue uploadInfo : genericFilesData) {
                if ("header".equals(uploadInfo.getKey())) {
                    continue;
                }
                String pathOnFS = uploadInfo.getValue().get("content");
                deleteTempFileFromFS(pathOnFS);
            }
            throw e;
        }
        return genericFilesData;
    }


    private MlKeyValue parseKeyValue(Path tmpUploadDir, JsonParser jsonParser) throws IOException {
        if (tmpUploadDir == null) {
            throw new NullPointerException("tmpUploadDir is marked non-null but is null");
        }
        MlKeyValue mlKeyValue = new MlKeyValue();
        while (jsonParser.nextToken() != JsonToken.END_OBJECT) {
            String field = jsonParser.getCurrentName();
            jsonParser.nextToken();
            if ("key".equals(field)) {
                mlKeyValue.setKey(jsonParser.getValueAsString());
                continue;
            }
            if ("value".equals(field)) {
                mlKeyValue.setValue(readValueAsMap(jsonParser, tmpUploadDir));
            }
        }
        return mlKeyValue;
    }

    @VisibleForTesting
    public void deleteTempFileFromFS(String pathOnFS) throws IOException {
        Files.deleteIfExists(Paths.get(pathOnFS));
    }

    private MlKeyValue uploadStream(InputStream bodyStream, Set<String> uploadedFiles, String storageId, String repositoryId, String organization, String modelName, String revision, String subRevisionFolder) {
        try {
            // 创建一个临时目录
            Path tempDir = Files.createTempDirectory(TEMP_UPLOAD_DIR);
            List<MlKeyValue> mlKeyValues = extractParamsFromJson(bodyStream, tempDir);
            return uploadEntries(mlKeyValues, storageId, repositoryId, organization, modelName, revision, subRevisionFolder, uploadedFiles);
        } catch (IOException e) {
            log.warn("Failed to parse commit request body for repoKey {} organization {} modelName: {}, revision: {}. Message {}", repositoryId, organization, modelName, revision, e.getMessage());
            log.debug("Failed to parse commit request body for repoKey {} organization {} modelName: {}, revision: {}", repositoryId, organization, modelName, revision, e);
            throw new RuntimeException("Failed to parse commit request body", e);
        }
    }

    @Nullable
    private MlKeyValue uploadEntries(List<MlKeyValue> entriesToUpload, String storageId, String repositoryId, String organization, String modelName, String revision, String subRevision, Set<String> uploadedFiles) throws IOException {
        MlKeyValue commitInfo = null;
        for (MlKeyValue uploadInfo : entriesToUpload) {
            if (uploadInfo.getValue() == null || uploadInfo.getValue().isEmpty()) {
                log.warn("Received commit request with an empty value for repoKey {} organization {} modelName: {}, revision: {}", repositoryId, organization, modelName, revision);
                throw new RuntimeException("Received commit request with empty upload value");
            }
            switch (uploadInfo.getKey()) {
                case "header":
                    commitInfo = uploadInfo;
                    continue;
                case "file":
                    processGenericFileUpload(storageId, repositoryId, organization, modelName, revision, subRevision, uploadedFiles, uploadInfo);
                    continue;
                case "lfsFile":
                    processLfsFileUpload(storageId, repositoryId, organization, modelName, revision, subRevision, uploadedFiles, uploadInfo);
                    continue;
            }
            log.warn("Received commit request with unsupported key: {} for repoKey {} organization {} modelName: {}, revision: {}", uploadInfo.getKey(), repositoryId, organization, modelName, revision);
        }
        return commitInfo;
    }

    private void processLfsFileUpload(String storageId, String repositoryId, String organization, String modelName, String revision, String subRevision, Set<String> uploadedFiles, MlKeyValue uploadInfo) {
        String oid = uploadInfo.getValue().get("oid");
        String fileName = uploadInfo.getValue().get("path");
        String path = MlModelUtils.getFilePath(organization, modelName, revision, subRevision, fileName);
        uploadedFiles.add(path);
        String oidFilePath = MlModelUtils.getLfsTmpUploadPath(organization, modelName, oid);
        Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, oidFilePath);

        if (artifact == null) {
            log.warn("No content for oid {} found for repo {}, organization {}, modelName {}, revision {}", oid, repositoryId, organization, modelName, revision);
        } else {

            String sourcePath = artifact.getArtifactPath();
            String destinationPath = MlModelUtils.getFilePath(organization, modelName, revision, subRevision, fileName);
            log.debug("Copying file with oid {} for repo {}, organization {}, modelName {}, revision {}", oid, repositoryId, organization, modelName, revision);
            //this.repositoryService.copy(repoKey, repoKey, sourcePath, destinationPath);
            RepositoryPath srcPath = repositoryPathResolver.resolve(storageId, repositoryId, sourcePath);
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repositoryId, destinationPath);
            try (InputStream inputStream = Files.newInputStream(srcPath);
                 BufferedInputStream bufferedInputStream = new BufferedInputStream(inputStream)) {
                artifactManagementService.validateAndStore(destPath, bufferedInputStream);
            } catch (IOException | ProviderImplementationException | ArtifactCoordinatesValidationException e) {
                log.error("Failed to copy file with oid {} for repo {}, organization {}, modelName {}, revision {}", oid, repositoryId, organization, modelName, revision);
                throw new RuntimeException(e);
            } finally {
                log.info("Failed to copy file with oid {} for repo {}, organization {}, modelName {}, revision {}", oid, repositoryId, organization, modelName, revision);
            }
        }
    }


    private void processGenericFileUpload(String storageId, String repositoryId, String organization, String modelName, String revision, String subRevision, Set<String> uploadedFiles, MlKeyValue uploadInfo) throws IOException {
        String path = MlModelUtils.getFilePath(organization, modelName, revision, subRevision, uploadInfo
                .getValue().get("path"));
        log.debug("Processing file {} for repoKey {}, organization {}, modelName {}, revision {}", path, repositoryId, organization, modelName, revision);
        String encoding = uploadInfo.getValue().get("encoding");
        if (!"base64".equals(encoding)) {
            log.warn("Received commit request with unsupported encoding: {} for repoKey {} organization {} modelName: {}, revision: {}", encoding, repositoryId, organization, modelName, revision);
            throw new RuntimeException("Unsupported encoding: " + encoding);
        }
        uploadedFiles.add(path);
        String contentPath = uploadInfo.getValue().get("content");
        if (skipUploading(storageId, repositoryId, path, contentPath)) {
            return;
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        Path filePath = Paths.get(contentPath);
        try (InputStream inputStream = Files.newInputStream(filePath)) {
            artifactManagementService.validateAndStore(repositoryPath, inputStream);
        } catch (IOException | ProviderImplementationException | ArtifactCoordinatesValidationException e) {
            log.error("upload file error", e);
        } finally {
            Path tmpUploadPath = Paths.get(contentPath);
            Files.deleteIfExists(tmpUploadPath);
        }
    }

    private boolean skipUploading(String storageId, String repositoryId, String path, String contentPath) {

        try {

            Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, path);
            if (artifact == null) {
                return false;
            }
            String sha2 = MlModelUtils.sha2(contentPath);
            if (sha2 == null) {
                return false;
            }
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            String sha23 = MlModelUtils.sha2(repositoryPath.getPath());
            if (sha2.equals(sha23)) {
                log.debug("Skipping upload of file {} since it already exists in repo {}", path, repositoryId);
                return true;
            }
        } catch (Exception e) {
            log.debug("Artifact {} does not exist in repo {}", path, repositoryId);
        }
        return false;
    }

    private Map<String, String> readValueAsMap(JsonParser valueParser, Path tmpUploadDir) throws IOException {
        Map<String, String> map = new HashMap<>();
        if (valueParser.getCurrentToken() == JsonToken.START_OBJECT) {
            while (valueParser.nextToken() != JsonToken.END_OBJECT) {
                String key = valueParser.getCurrentName();
                valueParser.nextToken();
                if ("content".equals(key)) {
                    String filePath = String.join("/", tmpUploadDir.toAbsolutePath().toString(), "huggingface_" + UUID.randomUUID());
                    try {
                        FileOutputStream fos = new FileOutputStream(filePath);
                        try {
                            valueParser.readBinaryValue(fos);
                            map.put(key, filePath);
                            fos.close();
                        } catch (Throwable throwable) {
                            try {
                                fos.close();
                            } catch (Throwable throwable1) {
                                throwable.addSuppressed(throwable1);
                            }
                            throw throwable;
                        }
                    } catch (Exception e) {
                        log.warn("error during content parsing. Message {}", e.getMessage());
                        log.debug("error during content parsing.", e);
                    }
                    continue;
                }
                map.put(key, valueParser.getText());
            }
        }
        return map;
    }

    public void processUploadComplete(MlModelRequestContext requestContext, String subRevisionFolder) {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        String fullModelName = MlModelUtils.getModelId(requestContext.getOrg(), requestContext.getModelName());
        log.debug("Starting MlModel index calculation repo: {}, model name is: {}", requestContext.getRepositoryId(), fullModelName);
        try {
            createdAndUploadLeadFile(requestContext, subRevisionFolder);
        } catch (Exception e) {
            log.error("Error while processing upload complete for repo: {}, model name is: {}", requestContext.getRepositoryId(), fullModelName, e);
        }
        log.debug("MlModel index calculation ended successfully for repo: {}, model name is: {}", requestContext.getRepositoryId(), fullModelName);
    }

    private void createdAndUploadLeadFile(MlModelRequestContext requestContext, String subRevisionFolder) throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {
        String repositoryId = requestContext.getRepositoryId();
        String organization = requestContext.getOrg();
        String modelName = requestContext.getModelName();
        String revision = requestContext.getRevision();
        RevisionData dataToSerialize = getDataToSerialize(subRevisionFolder, requestContext.getStorageId(), repositoryId, organization, modelName, revision);
        updateLeadFile(subRevisionFolder, requestContext.getStorageId(), repositoryId, organization, modelName, revision, dataToSerialize);
        if (MlModelUtils.isReleaseRevision(requestContext)) {
            RevisionData latestRevision;
            try {
                latestRevision = fetchRevision(requestContext, null);
            } catch (Exception packageException) {
                log.debug("Latest lead file for model: {}, revision: {} doesn't exist. It can happen on the first upload", modelName, revision);
                latestRevision = dataToSerialize;
            }
            String latestRevisionTimestamp = latestRevision.getLastModified();
            if (latestRevisionTimestamp != null &&
                    MlModelUtils.isIsoInstantFormat(latestRevisionTimestamp) && dataToSerialize
                    .getLastModified() != null &&
                    MlModelUtils.isIsoInstantFormat(dataToSerialize.getLastModified()) && !latestRevisionTimestamp.equals(subRevisionFolder)) {
                Date deletionDate, latestRevisionDate = MlModelUtils.convertToDate(latestRevisionTimestamp);
                Date uploadRevisionDate = MlModelUtils.convertToDate(dataToSerialize.getLastModified());
                if (latestRevisionDate.before(uploadRevisionDate) || latestRevisionDate.equals(uploadRevisionDate)) {
                    deletionDate = uploadRevisionDate;
                } else {
                    deletionDate = latestRevisionDate;
                }
                log.debug("Revision: {} is release version. going to calculate and delete old versions", requestContext.getRevision());
                deleteOldReleaseBranchSubRevisions(requestContext, deletionDate);
            }
        }
    }

    private void deleteOldReleaseBranchSubRevisions(MlModelRequestContext requestContext, Date subRevisionFolder) {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (subRevisionFolder == null) {
            throw new NullPointerException("subRevisionFolder is marked non-null but is null");
        }
        String modelRevisionPath = MlModelUtils.getModelRevisionPath(requestContext);
        String artifactRevisionTimestamp = MlModelUtils.extractSubRevisionFromPath(modelRevisionPath, StringUtils.isNotBlank(requestContext.getOrg()));
        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(requestContext.getStorageId(), requestContext.getRepositoryId(), modelRevisionPath);

            if (Files.exists(repositoryPath)) {
                artifactManagementService.delete(repositoryPath, false);
                log.info("Deleted old subRevision {} for repoKey {} organization {} modelName: {}, revision: {}", artifactRevisionTimestamp, requestContext.getRepositoryId(), requestContext
                        .getOrg(), requestContext.getModelName(), requestContext.getRevision());
            }
        } catch (IOException e) {
            log.warn("Failed to parse subRevision {} for repoKey {} organization {} modelName: {}, revision: {}. Message {}", artifactRevisionTimestamp, requestContext.getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision(), e.getMessage());
            log.debug("Failed to parse subRevision {} for repoKey {} organization {} modelName: {}, revision: {}", artifactRevisionTimestamp, requestContext.getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision(), e);
        }
    }

    private void updateLeadFile(String subRevisionFolder, String storageId, String repositoryId, String organization, String modelName, String revision, RevisionData dataToSerialize) throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {
        String leadFilePath = MlModelUtils.getFilePath(organization, modelName, revision, subRevisionFolder, ".folib_huggingface_model_info.json");
        Multimap<String, String> attributes = MlModelUtils.extractAttributesFromRevisionData(dataToSerialize, repositoryId, organization, modelName, revision);
        if (!attributes.isEmpty()) {
            log.debug("Setting attributes {} for repo {} model {} revision {} organization {}", attributes, repositoryId, modelName, revision, organization);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, leadFilePath);
            ObjectMapper objectMapper = MlModelUtils.createObjectMapper();
            try (ByteArrayInputStream is = new ByteArrayInputStream(objectMapper.enable(SerializationFeature.INDENT_OUTPUT).writeValueAsString(dataToSerialize).getBytes())) {
                artifactManagementService.validateAndStore(repositoryPath, is);
            }
            String latestLeadFilePath =  MlModelUtils.getLatestLeadFilePath(organization, modelName,revision);
            RepositoryPath repositoryLatestPath = repositoryPathResolver.resolve(storageId, repositoryId, latestLeadFilePath);
            try (ByteArrayInputStream is = new ByteArrayInputStream(objectMapper.enable(SerializationFeature.INDENT_OUTPUT).writeValueAsString(dataToSerialize).getBytes())) {
                artifactManagementService.validateAndStore(repositoryLatestPath, is);
            }
        }
    }



    @NonNull
    private RevisionData getDataToSerialize(String subRevisionFolder, String storageId, String repositoryId, String organization, String modelName, String revision) throws IOException {
        RevisionData dataToSerialize, revisionData = new RevisionData();
        revisionData.setCardData(new CardData());
        List<String> readmePaths = new ArrayList<>();
        revisionData.setSiblings(new ArrayList());
        String requestedRevisionPath = MlModelUtils.getFilePath(organization, modelName, revision, subRevisionFolder, "");
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, requestedRevisionPath);
        List<Path> fileList = new ArrayList<>();

        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            return null;
        }

        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    // 在这里可以处理目录（如果需要的话）
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (!file.getFileName().toString().startsWith(".")
                            && !file.getFileName().toString().endsWith(".metadata")
                            && !file.getFileName().toString().endsWith(".md5")
                            && !file.getFileName().toString().endsWith(".sha1")
                            && !file.getFileName().toString().endsWith(".sha256")
                            && !file.getFileName().toString().endsWith(".sm3")) {
                        fileList.add(file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }

        fileList.forEach(artifact -> {
            SiblingItem sibling = new SiblingItem();
            if ("readme.md".equals(artifact.getFileName().toString())) {
                readmePaths.add(artifact.toAbsolutePath().toString());
            }
            sibling.setFileName(artifact.getFileName().toString().replace(requestedRevisionPath, ""));
            revisionData.getSiblings().add(sibling);
        });

        if (!readmePaths.isEmpty()) {
            if (readmePaths.size() > 1) {
                log.warn("More than one readme file found for repoKey {}, organization {}, model {} revision {}. Only the first one will be used.", repositoryId, organization, modelName, revision);
            }
            String readmePath = readmePaths.get(0);
            try {
                //InputStream stream = this.downloadService.getStream(repositoryId, readmePath);
                byte[] packageJsonBytes = layoutProvider.getContentByFileName(repositoryPath, repositoryPath, readmePath);

                try (InputStream stream = new ByteArrayInputStream(packageJsonBytes)) {
                    dataToSerialize = MlModelIndexUtils.parseReadme(stream);
                    if (StringUtils.isBlank(dataToSerialize.getModelId())) {
                        dataToSerialize.setModelId(MlModelUtils.getModelId(organization, modelName));
                    }
                    dataToSerialize.setSiblings(revisionData.getSiblings());
                    log.info("Parsed readme file for repo {} model {} revision {} organization {} license {}", repositoryId, dataToSerialize
                            .getModelId(), revision, organization, dataToSerialize
                            .getCardData().getLicense());

                }
            } catch (Exception e) {
                log.error("Failed to parse readme file for repo {} model {} revision {} organization {}", repositoryId, modelName, revision, organization, e);
                dataToSerialize = revisionData;
            }
        } else {
            log.warn("No readme file found for repoKey {}, organization {}, model {} revision {}", repositoryId, organization, modelName, revision);
            dataToSerialize = revisionData;
        }
        dataToSerialize.setSha(MlModelUtils.getGeneratedCommitHash(revision, subRevisionFolder));
        dataToSerialize.setLastModified(subRevisionFolder);
        dataToSerialize.setTags(new LinkedList<>());
        dataToSerialize.setId(MlModelUtils.getModelId(organization, modelName));
        dataToSerialize.setPrivateProperty(false);
        dataToSerialize.setSiblings(dataToSerialize
                .getSiblings().stream().filter(sibling ->
                        !".folib_huggingface_model_info.json".equalsIgnoreCase(sibling.getFileName())).collect(Collectors.toList()));
        return dataToSerialize;
    }

    public String getRemoteUrl(Repository repository) {
        String remoteUrl = null;
        if (repository.isGroupRepository() && CollectionUtils.isNotEmpty(repository.getGroupRepositories())) {
            List<String> storageAndRepositoryIdList = Lists.newArrayList();
            configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
            for (String group : storageAndRepositoryIdList) {
                Repository groupRepository = configurationManager.getRepository(group);

                if (groupRepository.getRemoteRepository() != null) {
                    remoteUrl = groupRepository.getRemoteRepository() == null ? null : groupRepository.getRemoteRepository().getUrl();
                    break;
                }
            }
        } else if (repository.getRemoteRepository() != null) {
            remoteUrl = repository.getRemoteRepository().getUrl();
        }
        return remoteUrl;
    }
}
