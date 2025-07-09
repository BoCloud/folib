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
package com.folib.controllers.layout.docker;


import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.folib.domain.*;
import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.authentication.api.password.PasswordAuthentication;
import com.folib.components.layout.DockerComponent;
import com.folib.constant.GlobalConstants;
import com.folib.controllers.BaseArtifactController;
import com.folib.enums.DockerHeaderEnum;
import com.folib.enums.RepositoryScopeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.repositories.ArtifactRepository;
import com.folib.schema2.ImageManifest;
import com.folib.services.DirectoryListingService;
import com.folib.services.StorageManagementService;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.users.domain.Privileges;
import com.folib.users.security.JwtAuthenticationClaimsProvider;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.util.RepositoryPathUtil;
import com.folib.utils.FileUtils;
import com.folib.web.RepoMapping;
import io.swagger.annotations.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;


/**
 * REST API for all artifact-related processes.
 * <p>
 * Thanks to custom URL processing any path variable like '{artifactPath:.+}' will be processed as '**'.
 *
 * @author Martin Todorov
 * @author
 * @author veadan
 * @author @author veadan
 * @see{@linkplain http://docs.spring.io/spring/docs/current/spring-framework-reference/html/mvc.html#mvc-config-path-matching}
 */
@RestController
@Api(description = "docker坐标控制器", tags = "docker坐标控制器")
//@LayoutRequestMapping(DockerArtifactCoordinates.LAYOUT_NAME) docker工具访问接口路径从/v2开始，无法与/storages兼容
public class DockerArtifactController extends BaseArtifactController {

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Inject
    @Lazy
    private DockerComponent dockerComponent;

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private AuthenticationManager authenticationManager;

    @Inject
    private StorageManagementService storageManagementService;

    @Value("${server.ssl.enabled}")
    private boolean sslEnabled;

    /**
     * 文件进度
     */
    final ConcurrentHashMap<String, Long> ranges = new ConcurrentHashMap<String, Long>();

    /**
     * 镜像层 sha256 和分片 uuid
     */
    final ConcurrentHashMap<String, String> data = new ConcurrentHashMap<String, String>();

    /**
     * 检查终结点是否实现了 Docker 注册表 API V2。
     *
     * @param request
     * @param response
     * @return
     * @throws Exception
     */
    @ApiOperation(value = "v2")
    @RequestMapping(value = {"/v2/"}, method = {RequestMethod.GET})
    public ResponseEntity v2(@RequestHeader(value = HttpHeaders.AUTHORIZATION, required = false) String authorization,
                             @RequestHeader HttpHeaders httpHeaders,
                             HttpServletRequest request,
                             HttpServletResponse response)
            throws Exception {
        response.reset();
        response.setDateHeader("Date", System.currentTimeMillis());
        response.setHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
        setTokenUrl(request, response);
        if (Objects.isNull(authorization)) {
            return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
        }
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @ApiOperation(value = "v2 token")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "token."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @RequestMapping(value = {"/v2/token"}, method = {RequestMethod.GET, RequestMethod.POST})
    public ResponseEntity<Object> token(HttpServletRequest request, HttpServletResponse response, @RequestParam(value = "username", required = false) String username, @RequestParam(value = "password", required = false) String password, @RequestParam(value = "scope", required = false) String scope) {
        try {
            SecurityContext securityContext = SecurityContextHolder.getContext();
            Authentication authentication = securityContext.getAuthentication();
            if (Objects.isNull(authentication)) {
                return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
            }
            response.setHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
            int expireSeconds = 7200;
            if (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) {
                //Oauth2
                authenticationManager.authenticate(new PasswordAuthentication(username, password));
                String token = userService.generateSecurityToken(username, expireSeconds);
                JSONObject resultData = new JSONObject();
                resultData.put("access_token", token);
                resultData.put("scope", scope);
                resultData.put("expires_in", expireSeconds);
                return ResponseEntity.ok(resultData);
            }
            if (authentication instanceof AnonymousAuthenticationToken) {
                username = authentication.getPrincipal().toString();
                Map<String, String> claimMap = Collections.singletonMap(GlobalConstants.ANONYMOUS_TOKEN_KEY, username);
                JSONObject resultData = new JSONObject();
                String token = securityTokenProvider.getToken(username, claimMap, expireSeconds, null);
                resultData.put("token", token);
                resultData.put("expires_in", expireSeconds);
                return ResponseEntity.ok(resultData);
            }
            if (authentication.getPrincipal() instanceof SpringSecurityUser) {
                SpringSecurityUser springSecurityUser = (SpringSecurityUser) authentication.getPrincipal();
                Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
                JSONObject resultData = new JSONObject();
                String token = securityTokenProvider.getToken(springSecurityUser.getUsername(), claimMap, expireSeconds, null);
                resultData.put("token", token);
                resultData.put("expires_in", expireSeconds);
                return ResponseEntity.ok(resultData);
            }
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("authentication type error");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    /**
     * 校验层级是否已经存在
     *
     * @param httpHeaders
     * @param request
     * @param response
     */
    @ApiOperation(value = "Existing Layers 现有层")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "digest", value = "digest", required = true)
    })
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/{digest}", "/v2/{storageId}/{repositoryId}/{name}/**/blobs/{digest}"}, method = {RequestMethod.HEAD}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity existingLayers(@RequestHeader HttpHeaders httpHeaders,
                                         HttpServletRequest request,
                                         HttpServletResponse response,
                                         @PathVariable String storageId,
                                         @PathVariable String repositoryId,
                                         @PathVariable String name,
                                         @PathVariable String digest
    ) {
        try {
            String extractPath = getExtractPath(request);
            if (StringUtils.isNotBlank(extractPath)) {
                extractPath = extractPath.replace(String.format("/blobs/%s", digest), "");
            }
            response.reset();
            response.addHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
            response.addHeader("Accept-Ranges", "bytes");
            response.addHeader(DockerHeaderEnum.DOCKER_CONTENT_DIGEST.key(), digest);
            String imagePath = resolveImagePath(name, extractPath);
            String artifactPath = String.format("blobs/%s", digest);
            if (!validatePathPrivileges(storageId, repositoryId, Collections.singletonList(imagePath), Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
                setTokenUrl(request, response);
                return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
            }
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            boolean exists = artifactRealExists(repositoryPath);
            logger.info("StorageId [{}] repositoryId [{}] name [{}] extractPath [{}] imagePath [{}] artifactPath [{}] exists [{}]", storageId, repositoryId, name, extractPath, imagePath, artifactPath, exists);
            //200已经存在 404不存在
            if (exists) {
                long size = Files.size(repositoryPath);
                logger.info("StorageId [{}] repositoryId [{}] name [{}] extractPath [{}] imagePath [{}] artifactPath [{}] size [{}]", storageId, repositoryId, name, extractPath, imagePath, artifactPath, size);
                response.addHeader(DockerHeaderEnum.CONTENT_LENGTH.key(), size + "");
                response.addHeader(DockerHeaderEnum.STREAM_CONTENT_TYPE.key(), DockerHeaderEnum.STREAM_CONTENT_TYPE.value());
                return new ResponseEntity<>("OK", HttpStatus.OK);
            } else {
                return new ResponseEntity<>(HttpStatus.NOT_FOUND);
            }
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    /**
     * Starting An Upload 开始上传
     *
     * @param httpHeaders
     * @param request
     * @param response
     */
    @ApiOperation(value = "Starting An Upload 开始上传")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/", "/v2/{storageId}/{repositoryId}/{name}/**/blobs/uploads/"}, method = {RequestMethod.POST}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity<Object> startingAnUpload(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageId,
                                                   @PathVariable String repositoryId,
                                                   @PathVariable String name,
                                                   @RequestParam(required = false) String from,
                                                   @RequestParam(required = false) String mount

    ) {
        try {
            String extractPath = getExtractPath(request);
            if (StringUtils.isNotBlank(extractPath)) {
                extractPath = extractPath.replace("/blobs/uploads", "");
            }
            String imagePath = resolveImagePath(name, extractPath);
            if (!validatePathPrivileges(storageId, repositoryId, Collections.singletonList(imagePath), Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                setTokenUrl(request, response);
                return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
            }
            Storage storage = getStorage(storageId);
            if (Objects.isNull(storage)) {
                return new ResponseEntity<>(errMsg("NAME_UNKNOWN", GlobalConstants.STORAGE_NOT_FOUND_MESSAGE), HttpStatus.NOT_FOUND);
            }
            Repository repository = storage.getRepository(repositoryId);
            if (Objects.isNull(repository)) {
                return new ResponseEntity<>(errMsg("NAME_UNKNOWN", GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE), HttpStatus.NOT_FOUND);
            }
            String uuid = UUID.randomUUID().toString();
            String url = new StringBuffer().append(request.getRequestURI()).append(uuid).toString();
            response.reset();
            response.setDateHeader("Date", System.currentTimeMillis());
            response.setHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
            response.setHeader(DockerHeaderEnum.DOCKER_UPLOAD_UUID.key(), uuid);
            response.setHeader(DockerHeaderEnum.LOCATION.key(), url);
            response.setHeader(DockerHeaderEnum.CONTENT_LENGTH.key(), "0");
            return new ResponseEntity<>(HttpStatus.ACCEPTED);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }

    @ApiOperation(value = "Chunked Upload 分片上传")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/{uuid}", "/v2/{storageId}/{repositoryId}/{name}/**/blobs/uploads/{uuid}"}, method = {RequestMethod.PATCH}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> chunkedUpload(
            @RepoMapping Repository repository,
            @RequestHeader HttpHeaders httpHeaders,
            HttpServletRequest request,
            HttpServletResponse response,
            @PathVariable String storageId,
            @PathVariable String repositoryId,
            @PathVariable String name,
            @PathVariable String uuid


    ) throws Exception {
        InputStream inputStream = request.getInputStream();
        response.setCharacterEncoding("utf8");
        String extractPath = getExtractPath(request);
        if (StringUtils.isNotBlank(extractPath)) {
            extractPath = extractPath.replace(String.format("/blobs/uploads/%s", uuid), "");
        }
        String imagePath = resolveImagePath(name, extractPath);
        String targetFilePath = FileUtils.getBasePath() + storageId + "/" + repositoryId + "/" + imagePath + "/" + uuid;
        Path targetPath = Path.of(targetFilePath);
        Files.createDirectories(targetPath.getParent());
        Files.copy(inputStream, targetPath, StandardCopyOption.REPLACE_EXISTING);
        long fileSize = Files.size(targetPath);
        updateRanges(uuid, fileSize);
        String url = request.getRequestURI();
        response.reset();
        response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
        response.setHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
        response.setHeader(DockerHeaderEnum.DOCKER_UPLOAD_UUID.key(), uuid);
        response.setHeader(DockerHeaderEnum.LOCATION.key(), url);
        response.setHeader(DockerHeaderEnum.RANGE.key(), "0-" + (getRanges().getOrDefault(uuid, 1L) - 1));
        response.setHeader(DockerHeaderEnum.CONTENT_LENGTH.key(), "0");
        //202 Accepted
        return new ResponseEntity<>(HttpStatus.ACCEPTED);
    }

    @ApiOperation(value = "Upload Progress 上传进度")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/{uuid}", "/v2/{storageId}/{repositoryId}/{name}/**/blobs/uploads/{uuid}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity<String> uploadProgress(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageId,
                                                 @PathVariable String repositoryId,
                                                 @PathVariable String name,
                                                 @PathVariable String uuid

    ) {
        response.setHeader(DockerHeaderEnum.LOCATION.key(), request.getRequestURI());
        response.setHeader(DockerHeaderEnum.RANGE.key(), "0-" + (getRanges().getOrDefault(uuid, 1L) - 1));
        response.setHeader(DockerHeaderEnum.DOCKER_UPLOAD_UUID.key(), uuid);
        return new ResponseEntity<>("OK", HttpStatus.NO_CONTENT);
    }

    @ApiOperation(value = "Monolithic Upload 单片上传、Completed Upload 上传完成")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/{uuid}", "/v2/{storageId}/{repositoryId}/{name}/**/blobs/uploads/{uuid}"}, method = {RequestMethod.PUT}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<Object> monolithicUpload(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageId,
                                                   @PathVariable String repositoryId,
                                                   @PathVariable String name,
                                                   @PathVariable String uuid,
                                                   @RequestParam String digest

    ) {
        ResponseEntity result = new ResponseEntity<>(HttpStatus.CREATED);
        String imagePath = "";
        InputStream inputStream = null;
        try {
            String extractPath = getExtractPath(request);
            if (StringUtils.isNotBlank(extractPath)) {
                extractPath = extractPath.replace(String.format("/blobs/uploads/%s", uuid), "");
            }
            imagePath = resolveImagePath(name, extractPath);
            if (!validatePathPrivileges(storageId, repositoryId, Collections.singletonList(imagePath), Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                setTokenUrl(request, response);
                return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
            }
            int totalBytes = request.getContentLength();
            inputStream = request.getInputStream();
            if (totalBytes <= 0 || inputStream.available() == 0) {
                totalBytes = getRanges().getOrDefault(uuid, 1L).intValue();
                updateData(digest, uuid);
                inputStream = getInputStreamData(storageId, repositoryId, imagePath, digest);
            }
            String artifactPath = String.format("blobs/%s", digest);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            logger.info("StorageId [{}] repositoryId [{}] name [{}] imagePath [{}] digest [{}] uuid [{}] artifactPath [{}]", storageId, repositoryId, name, imagePath, digest, uuid, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, inputStream);

            String url = request.getRequestURI();
            url = url.replace("uploads/", "").replace(uuid, digest);
            response.reset();
            response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
            response.setHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
            response.setHeader(DockerHeaderEnum.LOCATION.key(), url);
            response.setHeader(DockerHeaderEnum.DOCKER_CONTENT_DIGEST.key(), digest);
            response.setHeader(DockerHeaderEnum.CONTENT_RANGE.key(), "0-" + (totalBytes - 1));
            response.setHeader(DockerHeaderEnum.CONTENT_LENGTH.key(), "0");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            if (e instanceof ArtifactStorageException) {
                result = new ResponseEntity<>(errMsg("DENIED", e.getMessage()), HttpStatus.FORBIDDEN);
                return result;
            }
            result = ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            deleteLayers(storageId, repositoryId, imagePath, digest, uuid);
        }
        return result;
    }

    /**
     * Pushing an Image Manifest
     *
     * @param httpHeaders
     * @param request
     * @param response
     * @param storageId
     * @param repositoryId
     * @param name
     * @param reference
     */
    @ApiOperation(value = "pushing An Image Manifest 推送镜像清单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "reference", value = "reference", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/manifests/{reference}", "/v2/{storageId}/{repositoryId}/{name}/**/manifests/{reference}"}, method = {RequestMethod.PUT})
    public ResponseEntity pushingAnImageManifest(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageId,
                                                 @PathVariable String repositoryId,
                                                 @PathVariable String name,
                                                 @PathVariable String reference
    ) {
        String manifestSha256 = null;
        ResponseEntity result = ResponseEntity.status(HttpStatus.CREATED).build();
        try {
            String extractPath = getExtractPath(request);
            if (StringUtils.isNotBlank(extractPath)) {
                extractPath = extractPath.replace(String.format("/manifests/%s", reference), "");
            }
            String imagePath = resolveImagePath(name, extractPath);
            if (!validatePathPrivileges(storageId, repositoryId, Collections.singletonList(imagePath), Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                setTokenUrl(request, response);
                return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
            }
            InputStream inputStream = request.getInputStream();
            byte[] bytes = inputStream.readAllBytes();
            logger.info("StorageId [{}] repositoryId [{}] name [{}] imagePath [{}] reference [{}] size [{}]", storageId, repositoryId, name, imagePath, reference, bytes.length);
            manifestSha256 = handlerManifest(storageId, repositoryId, imagePath, reference, bytes);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            if (e instanceof ArtifactStorageException) {
                result = new ResponseEntity<>(errMsg("DENIED", e.getMessage()), HttpStatus.FORBIDDEN);
                return result;
            }
            result = ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        response.reset();
        response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
        response.addHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
        response.addHeader(DockerHeaderEnum.DOCKER_CONTENT_DIGEST.key(), manifestSha256);
        response.addHeader(DockerHeaderEnum.CONTENT_LENGTH.key(), "0");
        return result;
    }

    /**
     * 处理manifest文件
     *
     * @param storageId    存储空间
     * @param repositoryId 所属仓库
     * @param name         tag
     * @param reference    sha256
     * @param bytes        文件字节数组
     * @return manifest sha256
     */
    String handlerManifest(String storageId,
                           String repositoryId,
                           String name,
                           String reference,
                           byte[] bytes) throws Exception {
        String manifestSha256 = null, directory;
        String manifestString = new String(bytes, StandardCharsets.UTF_8);
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        try (InputStream stream = new ByteArrayInputStream(bytes); InputStream destStream = new ByteArrayInputStream(bytes)) {
            boolean isTag = false;
            if (!reference.startsWith("sha256:")) {
                //Use SHA-256 algorithm
                MessageDigest shaDigest = MessageDigest.getInstance("SHA-256");
                //SHA-256 checksum
                String shaChecksum = getFileChecksum(shaDigest, new ByteArrayInputStream(bytes));
                manifestSha256 = String.format("sha256:%s", shaChecksum);
                directory = reference;
                isTag = true;
            } else {
                manifestSha256 = reference;
                directory = "manifest";
            }
            String artifactPath = String.format("manifest/%s", manifestSha256);
            //判断镜像清单是否在
            if (!mirrorLayerExists(artifactPath, storageId, repositoryId)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                dockerComponent.provideArtifact(repositoryPath);
                artifactManagementService.validateAndStore(repositoryPath, stream);
            }
            if (!isTag) {
                return manifestSha256;
            }
            String destArtifactPath = String.format("%s/%s/%s", name, directory, manifestSha256);
            logger.info("Docker tag repositoryPath：[{}]", destArtifactPath);
            String artifactName = String.format("%s/%s", name, directory);

            //判断镜像清单是否发生变化
            String tagSha256 = verifyTagSha256(artifactName, storageId, repositoryId);
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repositoryId, destArtifactPath);

            //如果存在并发生变化删除更新
            if (Objects.isNull(tagSha256)) {
                dockerComponent.provideArtifact(destPath);
                artifactManagementService.validateAndStore(destPath, destStream);
            } else if (!Objects.equals(tagSha256, manifestSha256)) {
                dockerComponent.provideArtifact(destPath);
                artifactManagementService.validateAndStore(destPath, destStream);
                RepositoryPath deletePath = repositoryPathResolver.resolve(storageId, repositoryId, destArtifactPath.replace(manifestSha256, tagSha256));
                artifactManagementService.delete(deletePath, true);
            }
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            throw e;
        }
        return manifestSha256;
    }


    private String getFileChecksum(MessageDigest digest, InputStream stream) throws IOException {
        //Get file input stream for reading the file content
        //FileInputStream fis = new FileInputStream(file);

        //Create byte array to read data in chunks
        byte[] byteArray = new byte[4096];
        int bytesCount = 0;

        //Read file data and update in message digest
        while ((bytesCount = stream.read(byteArray)) != -1) {
            digest.update(byteArray, 0, bytesCount);
        }
        ;

        //close the stream; We don't need it now.
        stream.close();

        //Get the hash's bytes
        byte[] bytes = digest.digest();

        //This bytes[] has bytes in decimal format;
        //Convert it to hexadecimal format
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < bytes.length; i++) {
            sb.append(Integer.toString((bytes[i] & 0xff) + 0x100, 16).substring(1));
        }

        //return complete hash
        return sb.toString();
    }

    /**
     * 删除临时目录文件
     *
     * @param storageId
     * @param repositoryId
     * @param name
     * @param digests
     * @param uuid
     */
    public void deleteLayers(String storageId,
                             String repositoryId,
                             String name,
                             String digests,
                             String uuid) {
        FileUtils utils = new FileUtils();
        if (getData().containsKey(digests) && Objects.nonNull(getData().get(digests))) {
            String fileDir = String.format("%s/%s/%s", storageId, repositoryId, name);
            utils.deleteDir(fileDir, getData().get(digests));
            removeData(digests);
            removeRanges(uuid);
        }
    }

    /**
     * 存储数据
     *
     * @param storageId
     * @param repositoryId
     * @param name
     * @param digest
     * @return
     */
    public InputStream getInputStreamData(String storageId, String repositoryId, String name, String digest) {
        FileUtils utils = new FileUtils();
        String fileDir = String.format("%s/%s/%s", storageId, repositoryId, name);
        String fileName = getData().get(digest);
        return utils.getFile(fileDir, fileName);
    }

    @ApiOperation(value = "Existing Manifests 现有清单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "tag", value = "tag", required = true)
    })
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/manifests/{reference}", "/v2/{storageId}/{repositoryId}/{name}/**/manifests/{reference}"}, method = {RequestMethod.HEAD}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity existingManifests(@RequestHeader HttpHeaders httpHeaders,
                                            HttpServletRequest request,
                                            HttpServletResponse response,
                                            @PathVariable String storageId,
                                            @PathVariable String repositoryId,
                                            @PathVariable String name,
                                            @PathVariable String reference) throws Exception {
        String extractPath = getExtractPath(request);
        if (StringUtils.isNotBlank(extractPath)) {
            extractPath = extractPath.replace(String.format("/manifests/%s", reference), "");
        }
        String imagePath = resolveImagePath(name, extractPath);
        if (!validatePathPrivileges(storageId, repositoryId, Collections.singletonList(imagePath), Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
            setTokenUrl(request, response);
            return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
        }
        ResponseEntity entity = ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        String digest = "", contentLength = "0";
        response.reset();
        RepositoryPath repositoryPath = dockerComponent.resolveManifest(storageId, repositoryId, imagePath, reference);
        if (artifactRealExists(repositoryPath)) {
            entity = ResponseEntity.status(HttpStatus.OK).build();
            contentLength = Long.toString(repositoryPath.getArtifactEntry().getSizeInBytes());
            ImageManifest imageManifest = JSON.parseObject(Files.readString(repositoryPath), ImageManifest.class);
            response.addHeader(DockerHeaderEnum.DOCKER_CONTENT_TYPE.key(), imageManifest.getMediaType());
            digest = repositoryPath.getFileName().toString();
        }
        response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
        response.addHeader(DockerHeaderEnum.RATE_LIMIT_LIMIT.key(), "100;w=21600");
        response.addHeader(DockerHeaderEnum.RATE_LIMIT_REMAINING.key(), "100;w=21600");
        response.addHeader(DockerHeaderEnum.STRICT_TRANSPORT_SECURITY.key(), "max-age=31536000");
        response.addHeader(DockerHeaderEnum.DOCKER_CONTENT_DIGEST.key(), digest);
        response.addHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
        response.addHeader(DockerHeaderEnum.DOCKER_RATE_LIMIT_SOURCE.key(), request.getRemoteHost());
        response.addHeader(DockerHeaderEnum.ETAG.key(), digest);
        response.addHeader(DockerHeaderEnum.CONTENT_LENGTH.key(), contentLength);
        return entity;
    }

    public String verifyTagSha256(String artifactName, String storageId, String repositoryId) {
        try {
            return getLayers(artifactName, storageId, repositoryId);
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * 是否存在镜像层
     *
     * @return true 存在 false 不存在
     */
    private Boolean mirrorLayerExists(String artifactName, String storageId, String repositoryId) throws IOException {
        return artifactRealExists(repositoryPathResolver.resolve(storageId, repositoryId, artifactName));
    }

    private String getLayers(String artifactName, String storageId, String repositoryId) throws IOException {
        Artifact artifacts = getArtifact(artifactName, storageId, repositoryId);
        String layers = null;
        if (Objects.nonNull(artifacts)) {
            Map<String, String> mapCoordinates = artifacts.getArtifactCoordinates().getCoordinates();

            if (Objects.nonNull(mapCoordinates) && mapCoordinates.containsKey("layers")) {
                layers = mapCoordinates.get("layers");
            }
        }
        return layers;
    }

    public Artifact getArtifact(String artifactName, String storageId, String repositoryId) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactName);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
        List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.include(file.getName())).collect(Collectors.toList());
        FileContent fileContent = fileContents.get(0);
        String artifactPath = fileContent.getArtifactPath();
        return artifactRepository.findOneArtifact(storageId, repositoryId, artifactPath);
    }


    @ApiOperation(value = "Pulling an Image Manifest 获取镜像清单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "digest", value = "digest", required = true)
    })
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/manifests/{digest}", "/v2/{storageId}/{repositoryId}/{name}/**/manifests/{digest}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity pullingAnImageManifest(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageId,
                                                 @PathVariable String repositoryId,
                                                 @PathVariable String name,
                                                 @PathVariable String digest) {
        String extractPath = getExtractPath(request);
        if (StringUtils.isNotBlank(extractPath)) {
            extractPath = extractPath.replace(String.format("/manifests/%s", digest), "");
        }
        String imagePath = resolveImagePath(name, extractPath);
        if (!validatePathPrivileges(storageId, repositoryId, Collections.singletonList(imagePath), Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
            setTokenUrl(request, response);
            return new ResponseEntity<>(unAuth(), HttpStatus.UNAUTHORIZED);
        }
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return new ResponseEntity<>(errMsg("NAME_UNKNOWN", GlobalConstants.STORAGE_NOT_FOUND_MESSAGE), HttpStatus.NOT_FOUND);
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            return new ResponseEntity<>(errMsg("NAME_UNKNOWN", GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE), HttpStatus.NOT_FOUND);
        }
        ResponseEntity entity = new ResponseEntity<>(errMsg("MANIFEST_UNKNOWN", "The named manifest is not known to the registry."), HttpStatus.NOT_FOUND);
        try {
            String artifactPath = String.format("manifest/%s", digest);
            logger.debug("StorageId [{}] repositoryId [{}] name [{}] imagePath [{}] digest [{}] artifactPath [{}]", storageId, repositoryId, name, imagePath, digest, artifactPath);
            RepositoryPath repositoryPath = dockerComponent.resolveManifest(storageId, repositoryId, imagePath, digest);
            if (artifactRealExists(repositoryPath)) {
                vulnerabilityBlock(repositoryPath);
                response.reset();
                entity = ResponseEntity.status(HttpStatus.OK).build();
                digest = repositoryPath.getFileName().toString();
                response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
                response.addHeader(DockerHeaderEnum.DOCKER_CONTENT_DIGEST.key(), digest);
                response.addHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
                response.addHeader(DockerHeaderEnum.ETAG.key(), digest);
                provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            }
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        }
        return entity;
    }

    @ApiOperation(value = "Pulling a Layer 拉取镜像层")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "digest", value = "digest", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/{digest}", "/v2/{storageId}/{repositoryId}/{name}/**/blobs/{digest}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity pullingALayer(@RequestHeader HttpHeaders httpHeaders,
                                        HttpServletRequest request,
                                        HttpServletResponse response,
                                        @PathVariable String storageId,
                                        @PathVariable String repositoryId,
                                        @PathVariable String name,
                                        @PathVariable String digest) {
        String extractPath = getExtractPath(request);
        if (StringUtils.isNotBlank(extractPath)) {
            extractPath = extractPath.replace(String.format("/blobs/%s", digest), "");
        }
        String imagePath = resolveImagePath(name, extractPath);
        String artifactPath = String.format("blobs/%s", digest);
        ResponseEntity entity = ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            entity = new ResponseEntity<>(errMsg("NAME_UNKNOWN", GlobalConstants.STORAGE_NOT_FOUND_MESSAGE), HttpStatus.NOT_FOUND);
            return entity;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            entity = new ResponseEntity<>(errMsg("NAME_UNKNOWN", GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE), HttpStatus.NOT_FOUND);
            return entity;
        }
        try {
            logger.debug("StorageId [{}] repositoryId [{}] name [{}] imagePath [{}] digest [{}] artifactPath [{}]", storageId, repositoryId, name, imagePath, digest, artifactPath);
            String targetUrl = String.format("%s/blobs/%s", StringUtils.removeEnd(imagePath, "/"), digest);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!artifactRealExists(repositoryPath)) {
                repositoryPath.setTargetUrl(targetUrl);
                repositoryPath.setArtifactPath(imagePath);
                repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            }
            if (artifactRealExists(repositoryPath)) {
                vulnerabilityBlock(repositoryPath);
                response.reset();
                response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
                response.addHeader(DockerHeaderEnum.STREAM_CONTENT_TYPE.key(), DockerHeaderEnum.STREAM_CONTENT_TYPE.value());
                response.addHeader(DockerHeaderEnum.DOCKER_CONTENT_DIGEST.key(), digest);
                response.addHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
                provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
                entity = ResponseEntity.status(HttpStatus.OK).build();
            }
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        }
        return entity;
    }

    @ApiOperation(value = "Listing Image Tags 获取镜像tag列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "镜像名称", required = true),
            @ApiImplicitParam(name = "n", value = "返回个数"),
            @ApiImplicitParam(name = "last", value = "last")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/tags/list", "/v2/{storageId}/{repositoryId}/{name}/**/tags/list"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity<Object> listingImageTags(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageId,
                                                   @PathVariable String repositoryId,
                                                   @PathVariable String name,
                                                   @RequestParam(name = "n", required = false) Integer n,
                                                   @RequestParam(name = "last", required = false) String last) {
        String imagePath = "";
        try {
            String extractPath = getExtractPath(request);
            if (StringUtils.isNotBlank(extractPath)) {
                extractPath = extractPath.replace("/tags/list", "");
            }
            imagePath = resolveImagePath(name, extractPath);
            logger.info("Listing Image Tags storageId [{}] repositoryId [{}] imagePath [{}] n [{}] last [{}]", storageId, repositoryId, imagePath, n, last);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, imagePath);
            List<FileContent> tagDirList = null;
            if (Files.exists(repositoryPath)) {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                tagDirList = Optional.ofNullable(directoryListing.getDirectories()).orElse(Collections.emptyList()).stream().filter(item -> RepositoryPathUtil.isDockerTag(repositoryPathResolver.resolve(storageId, repositoryId, item.getArtifactPath()))).collect(Collectors.toList());
            }
            List<String> tagList = Optional.ofNullable(tagDirList).orElse(Collections.emptyList()).stream().map(FileContent::getName).collect(Collectors.toList());
            List<String> resultList;
            int size = tagList.size(), startIndex = 0, endIndex = size;
            if (StringUtils.isNotBlank(last)) {
                int index = tagList.indexOf(last);
                if (index != -1) {
                    startIndex = index + 1;
                }
            }
            if (startIndex > size) {
                startIndex = size;
            }
            if (Objects.nonNull(n)) {
                if (n < 1) {
                    n = size;
                }
                endIndex = startIndex + n;
            }
            if (endIndex > size) {
                endIndex = size;
            }
            String link = "";
            resultList = tagList.subList(startIndex, endIndex);
            if (CollectionUtils.isNotEmpty(resultList)) {
                last = resultList.get(resultList.size() - 1);
                if (Objects.nonNull(n) && n > 0 & endIndex <= size - 1) {
                    link = "</v2/%s/tags/list?last=%s&n=%s>; rel=\"next\"";
                    link = String.format(link, String.format("%s/%s/%s", storageId, repositoryId, imagePath), last, n);
                }
            }
            logger.info("Listing Image Tags storageId [{}] repositoryId [{}] imagePath [{}] startIndex [{}] endIndex [{}] link [{}]", storageId, repositoryId, imagePath, startIndex, endIndex, link);
            DockerTags dockerTags = DockerTags.builder().name(imagePath).tags(resultList).build();
            response.reset();
            response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
            response.addHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
            if (StringUtils.isNotBlank(link)) {
                response.addHeader(HttpHeaders.LINK, link);
            }
            return ResponseEntity.ok(dockerTags);
        } catch (Exception ex) {
            logger.error("Listing Image Tags storageId [{}] repositoryId [{}] imagePath [{}] n [{}] last [{}] error [{}]", storageId, repositoryId, imagePath, n, last, ExceptionUtils.getStackTrace(ex));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ex.getMessage());
        }
    }

    @ApiOperation(value = "Get Catalog 获取仓库列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "n", value = "返回个数"),
            @ApiImplicitParam(name = "last", value = "last")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/_catalog"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity<Object> getCatalog(@RequestHeader HttpHeaders httpHeaders,
                                             HttpServletRequest request,
                                             HttpServletResponse response,
                                             @RequestParam(name = "n", required = false) Integer n,
                                             @RequestParam(name = "last", required = false) String last,
                                             Authentication authentication) {
        try {
            logger.info("GET Catalog n [{}] last [{}]", n, last);
            List<Storage> storageList = new ArrayList<>(configurationManagementService.getConfiguration()
                    .getStorages()
                    .values());
            String username = "";
            if (Objects.nonNull(authentication)) {
                final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
                username = loggedUser.getUsername();
            }
            String link = "", next = "";
            List<String> resultList = Collections.emptyList(), dataList = Lists.newArrayList();
            if (CollectionUtil.isNotEmpty(storageList)) {
                //查询数据库中存储空间绑定的用户
                storageManagementService.getStorageUsers(storageList);
                boolean filterByUser = !hasAdmin();
                String finalUsername = username;
                storageList = storageList.stream()
                        .distinct()
                        .filter(s -> !filterByUser || (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(finalUsername)) ||
                                (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                        .collect(Collectors.toCollection(LinkedList::new));
                List<Repository> repositories;
                String dockerLevel = System.getProperty("DockerLevel");
                for (Storage storage : storageList) {
                    boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username));
                    repositories = new LinkedList<Repository>(storage.getRepositories().values());
                    repositories = repositories.stream().distinct()
                            .filter(r -> DockerLayoutProvider.ALIAS.equalsIgnoreCase(r.getLayout()))
                            .collect(Collectors.toCollection(LinkedList::new));
                    if (flag) {
                        repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()))).collect(Collectors.toList());
                    }
                    if (CollectionUtils.isNotEmpty(repositories)) {
                        repositories.forEach(repository -> {
                            try {
                                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId());
                                String prefix = String.format("%s/%s", repository.getStorage().getId(), repository.getId());
                                if (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath)) {
                                    if (GlobalConstants.DOCKER_LEVEL_SINGLE.equals(dockerLevel)) {
                                        DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                                        if (Objects.nonNull(directoryListing) && CollectionUtils.isNotEmpty(directoryListing.getDirectories())) {
                                            dataList.addAll(directoryListing.getDirectories().stream().filter(item -> StringUtils.isNotBlank(item.getName()) && !item.getName().startsWith(".")).map(item -> String.format("%s/%s", prefix, item.getName())).collect(Collectors.toList()));
                                        }
                                    } else {
                                        List<RepositoryPath> repositoryPathList = RepositoryPathUtil.getDockerImagePaths(repositoryPath);
                                        if (CollectionUtils.isNotEmpty(repositoryPathList)) {
                                            repositoryPathList.forEach(item -> {
                                                String path = "";
                                                try {
                                                    path = RepositoryFiles.relativizePath(item);
                                                } catch (Exception ex) {
                                                    logger.error(ExceptionUtils.getStackTrace(ex));
                                                }
                                                if (StringUtils.isNotBlank(path)) {
                                                    dataList.add(String.format("%s/%s", prefix, path));
                                                }
                                            });
                                        }
                                    }
                                }
                            } catch (Exception ex) {
                                logger.error("GET Catalog directory listing error [{}]", ExceptionUtils.getStackTrace(ex));
                            }
                        });
                    }
                }
                int size = dataList.size(), startIndex = 0, endIndex = size;
                if (StringUtils.isNotBlank(last)) {
                    int index = dataList.indexOf(last);
                    if (index != -1) {
                        startIndex = index + 1;
                    }
                }
                if (startIndex > size) {
                    startIndex = size;
                }
                if (Objects.nonNull(n)) {
                    if (n < 1) {
                        n = size;
                    }
                    endIndex = startIndex + n;
                }
                if (endIndex > size) {
                    endIndex = size;
                }
                resultList = dataList.subList(startIndex, endIndex);
                if (CollectionUtils.isNotEmpty(resultList)) {
                    last = resultList.get(resultList.size() - 1);
                    if (Objects.nonNull(n) && n > 0 & endIndex <= size - 1) {
                        link = "</v2/_catalog?last=%s&n=%s>; rel=\"next\"";
                        link = String.format(link, last, n);

                        next = "/v2/_catalog?last=%s&n=%s";
                        next = String.format(next, last, n);
                    }
                }
                logger.info("GET Catalog n [{}] last [{}] startIndex [{}] endIndex [{}] link [{}]", n, last, startIndex, endIndex, link);
            }
            DockerCatalog dockerCatalog = DockerCatalog.builder().next(next).repositories(resultList).build();
            response.reset();
            response.setDateHeader(DockerHeaderEnum.DATE.key(), System.currentTimeMillis());
            response.addHeader(DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerHeaderEnum.DOCKER_DISTRIBUTION_API_VERSION.value());
            if (StringUtils.isNotBlank(link)) {
                response.addHeader(HttpHeaders.LINK, link);
            }
            return ResponseEntity.ok(dockerCatalog);
        } catch (Exception ex) {
            logger.error("GET Catalog [n:{}, last:{} error {}]", n, last, ExceptionUtils.getStackTrace(ex));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ex.getMessage());
        }
    }

    private ConcurrentHashMap<String, Long> getRanges() {
        return ranges;
    }

    private void updateRanges(String uuid, long value) {
        if (ranges.containsKey(uuid)) {
            ranges.replace(uuid, ranges.get(uuid) + value);
        } else {
            ranges.put(uuid, value);
        }
    }

    private ConcurrentHashMap<String, String> getData() {
        return data;
    }

    private void updateData(String digest, String uuid) {
        if (data.containsKey(uuid)) {
            data.replace(digest, uuid);
        } else {
            data.put(digest, uuid);
        }
    }

    private void removeData(String digest) {
        data.remove(digest);
    }

    private void removeRanges(String uuid) {
        ranges.remove(uuid);
    }

    private Map<String, Object> unAuth() {
        Map<String, Object> result = new HashMap<>(1);
        Map<String, Object> resultData = new HashMap<>(1);
        resultData.put("code", "UNAUTHORIZED");
        resultData.put("message", "access to the requested resource is not authorized");
        resultData.put("detail", null);
        List<Map> list = new ArrayList<>();
        list.add(resultData);
        result.put("errors", list);
        return result;
    }

    private Map<String, Object> unForbidden(String storageId, String repositoryId) {
        Map<String, Object> result = new HashMap<>(1);
        Map<String, Object> resultData = new HashMap<>(1);
        resultData.put("code", "UNAUTHORIZED");
        resultData.put("message", String.format("access to the requested storage %s repository %s is forbidden", storageId, repositoryId));
        resultData.put("detail", null);
        List<Map> list = new ArrayList<>();
        list.add(resultData);
        result.put("errors", list);
        return result;
    }

    private Map<String, Object> errMsg(String code, String msg) {
        Map<String, Object> result = new HashMap<>(1);
        Map<String, Object> resultData = new HashMap<>(1);
        resultData.put("code", code);
        resultData.put("message", msg);
        resultData.put("detail", null);
        List<Map> list = new ArrayList<>();
        list.add(resultData);
        result.put("errors", list);
        return result;
    }

    private void setTokenUrl(HttpServletRequest request, HttpServletResponse response) {
        String originalProtocol = request.getHeader("X-Forwarded-Proto"), https = "https";
        if (sslEnabled || https.equals(originalProtocol)) {
            // 使用HTTPS协议
            response.setHeader("WWW-Authenticate", String.format("Bearer realm=\"%stoken\",service=\"%s\"", "https://" + request.getServerName() + "/v2/", request.getServerName()));
        } else {
            // 使用HTTP协议
            response.setHeader("WWW-Authenticate", String.format("Bearer realm=\"%stoken\",service=\"%s\"", "http://" + request.getServerName() + ":" + request.getServerPort() + "/v2/", request.getServerName() + ":" + request.getServerPort()));
        }
    }

    private String resolveImagePath(String name, String extractPath) {
        String imagePath = name;
        if (StringUtils.isNotBlank(extractPath)) {
            imagePath = imagePath + File.separator + extractPath;
        }
        return imagePath;
    }
}
