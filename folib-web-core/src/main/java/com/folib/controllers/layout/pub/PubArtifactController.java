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
package com.folib.controllers.layout.pub;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.google.common.collect.Maps;
import com.folib.artifact.coordinates.PubCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.constants.PubConstants;
import com.folib.controllers.BaseArtifactController;
import com.folib.domain.PubPackageMetadata;
import com.folib.domain.PubPackageVersionMetadata;
import com.folib.domain.PubUpload;
import com.folib.domain.Pubspec;
import com.folib.domain.common.StandardError;
import com.folib.domain.common.StandardResponse;
import com.folib.enums.PubIndexTypeEnum;
import com.folib.indexer.PubMetadataExtractor;
import com.folib.indexer.PubPackageMetadataIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.services.PubService;
import com.folib.storage.repository.Repository;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.javatuples.Pair;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;


/**
 * This Controller used to handle pub requests.
 *
 * @author veadan
 */
@RestController
@LayoutReqMapping(PubCoordinates.LAYOUT_NAME)
@Api(description = "pub仓库控制器", tags = "pub仓库控制器")
public class PubArtifactController
        extends BaseArtifactController {

    private static final String PACKAGES_ENDPOINT = "/api/packages/";

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private AuthenticationManager authenticationManager;

    @Inject
    private PubService pubService;

    @Inject
    @Lazy
    private PubPackageMetadataIndexer pubPackageMetadataIndexer;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @GetMapping(path = "{storageId}/{repositoryId}/api/packages/{packageName}/versions/{version}")
    @ApiOperation(value = "Inspect the version of a PUB package.", nickname = "inspectSpecificVersion", notes = "Deprecated as of Dart 2.8, use \"listAllVersions\" instead.")
    @ApiResponses({@ApiResponse(code = 200, message = "OK", response = PubPackageVersionMetadata.class), @ApiResponse(code = 403, message = "Forbidden. User has no read permission"), @ApiResponse(code = 404, message = "Package Not Found")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity inspectVersion(@RepoMapping Repository repository, @PathVariable(name = "storageId") String storageId, @PathVariable(name = "repositoryId") String repositoryId,
                                         @PathVariable("packageName") String packageName, @PathVariable("version") String version, HttpServletRequest request, HttpServletResponse response) {
        PubPackageVersionMetadata inspectedVersionMetadata = pubService.inspectVersion(repository, packageName, version, PACKAGES_ENDPOINT + packageName);
        if (Objects.isNull(inspectedVersionMetadata)) {
            String message = String.format("Could not find `package \"%s\" version \"%s\"`.", packageName, version);
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(StandardResponse.builder().code(GlobalConstants.NOT_FOUND).message(message).error(StandardError.builder().code(GlobalConstants.NOT_FOUND).message(message).build()).build());
        }
        response.setHeader("Content-Type", PubConstants.CONTENT_TYPE);
        return ResponseEntity.ok(JSON.toJSONString(inspectedVersionMetadata, SerializerFeature.PrettyFormat));
    }

    @GetMapping(path = "{storageId}/{repositoryId}/api/packages/{packageName}")
    @ApiOperation(value = "List all the versions of a PUB package.", nickname = "listAllVersions")
    @ApiResponses({@ApiResponse(code = 200, message = "OK", response = PubPackageMetadata.class), @ApiResponse(code = 403, message = "Forbidden. User has no read permission"), @ApiResponse(code = 404, message = "Package Not Found")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity packages(@RepoMapping Repository repository,
                                   @PathVariable(name = "storageId") String storageId,
                                   @PathVariable(name = "repositoryId") String repositoryId,
                                   @PathVariable(name = "packageName") String packageName,
                                   HttpServletResponse response) {
        JSONObject packageJson = pubService.packages(repository, packageName, PACKAGES_ENDPOINT + packageName);
        if (Objects.isNull(packageJson)) {
            String message = String.format("Could not find `package \"%s\"`.", packageName);
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(StandardResponse.builder().code(GlobalConstants.NOT_FOUND).message(message).error(StandardError.builder().code(GlobalConstants.NOT_FOUND).message(message).build()).build());
        }
        response.setHeader("Content-Type", PubConstants.CONTENT_TYPE);
        return ResponseEntity.ok(packageJson);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @ApiOperation(value = "Download a specific version of a PUB package.", nickname = "DownloadPackageVersion")
    @RequestMapping(path = "{storageId}/{repositoryId}/packages/{packageName}/versions/{artifactName}",
            method = {RequestMethod.GET,
                    RequestMethod.HEAD})
    public void download(@RepoMapping Repository repository,
                         @PathVariable(name = "storageId") String storageId,
                         @PathVariable(name = "repositoryId") String repositoryId,
                         @PathVariable(name = "packageName") String packageName,
                         @PathVariable(name = "artifactName") String artifactName,
                         @RequestHeader HttpHeaders httpHeaders,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        long startTime = System.currentTimeMillis();
        final String artifactPath = String.format("%s/%s", packageName, artifactName);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        logger.debug("Pub download [{}] task time [{}] ms", artifactPath, System.currentTimeMillis() - startTime);
    }

    @GetMapping(path = "{storageId}/{repositoryId}/api/packages/versions/new")
    @ApiOperation(value = "Start deploy process by retrieving the url for deployment.", nickname = "getUrlDeployment", response = PubUpload.class)
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity getUrlDeployment(@RepoMapping Repository repository,
                                           @PathVariable(name = "storageId") String storageId,
                                           @PathVariable(name = "repositoryId") String repositoryId,
                                           HttpServletResponse response) {
        String url = getRepositoryBaseUrl(repository) + "/deploy";
        Map<String, String> fields = Maps.newHashMap();
        fields.put("file", "file");
        PubUpload pubUpload = PubUpload.builder().url(url).fields(fields).build();
        response.setHeader("Content-Type", PubConstants.CONTENT_TYPE);
        return ResponseEntity.ok(pubUpload);
    }

    @PostMapping(path = "{storageId}/{repositoryId}/deploy", consumes = MediaType.MULTIPART_FORM_DATA)
    @ApiOperation(value = "Performs deploy process by uploading the package.", nickname = "deploy")
    @ApiResponses({@ApiResponse(code = 204, message = "No Content"), @ApiResponse(code = 400, message = "Bad Request"), @ApiResponse(code = 500, message = "Internal server error")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity deploy(@RepoMapping Repository repository,
                                 @PathVariable(name = "storageId") String storageId,
                                 @PathVariable(name = "repositoryId") String repositoryId,
                                 HttpServletRequest request,
                                 @RequestParam("file") MultipartFile file,
                                 HttpServletResponse response) throws Exception {
        PubMetadataExtractor extractor = new PubMetadataExtractor();
        try (InputStream fileInputStream = file.getInputStream()) {
            Pair<Pubspec, Path> pubspecPathPair = extractor.extractPubSpec(fileInputStream);
            try (InputStream bufferedInputStream = new BufferedInputStream(Files.newInputStream(extractor.extractPubSpec(file.getInputStream()).getValue1()))) {
                Pubspec pubspec = pubspecPathPair.getValue0();
                PubCoordinates pubArtifactCoordinates = PubCoordinates.of(pubspec.getName(), pubspec.getVersion(), PubCoordinates.PUB_EXTENSION);
                String artifactPath = pubArtifactCoordinates.convertToPath(pubArtifactCoordinates);
                logger.info("Pub upload storageId [{}] repositoryId [{}] artifactPath [{}]", storageId, repositoryId, artifactPath);
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                artifactManagementService.validateAndStore(repositoryPath, bufferedInputStream);
                pubPackageMetadataIndexer.indexAsSystem(repositoryPath, PubIndexTypeEnum.ADD);
            } catch (Exception e) {
                logger.error(e.getMessage(), e);
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
            } finally {
                Files.deleteIfExists(pubspecPathPair.getValue1());
            }
        } catch (IOException e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        response.setHeader("Location", getRepositoryBaseUrl(repository) + "/finalizeDeployment");
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

    @GetMapping(path = "{storageId}/{repositoryId}/finalizeDeployment")
    @ApiOperation(value = "Finalize the deploy process.", nickname = "finalizeDeployment")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity finalizeDeployment(@PathVariable(name = "storageId") String storageId,
                                             @PathVariable(name = "repositoryId") String repositoryId, HttpServletResponse response) {
        response.setHeader("Content-Type", PubConstants.CONTENT_TYPE);
        return ResponseEntity.ok(PubConstants.GET_FINALIZE_DEPLOYMENT_RESULT);
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(path = "{storageId}/{repositoryId}/{packageName}/{artifactName}")
    public void download(@RepoMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String packageName,
                         @PathVariable String artifactName,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String path = packageName + File.separator + artifactName;
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }
}
