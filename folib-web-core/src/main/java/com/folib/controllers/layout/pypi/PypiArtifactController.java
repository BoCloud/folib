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
package com.folib.controllers.layout.pypi;

import com.google.common.collect.Sets;
import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.components.PypiBrowsePackageHtmlResponseBuilder;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.controllers.BaseArtifactController;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.ArtifactIdGroupEntity;
import com.folib.providers.ProviderImplementationException;
import com.folib.providers.io.RepositoryPath;
import com.folib.services.PypiService;
import com.folib.storage.metadata.pypi.PypiArtifactMetadata;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.folib.utils.PypiPackageNameConverter;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Rest End Points for Pypi Artifacts requests.
 * This controller will be Entry point for various pip commands.
 *
 * @author veadan
 */
@RestController
@LayoutReqMapping(PypiCoordinates.LAYOUT_NAME)
@Api(description = "python坐标控制器", tags = "python坐标控制器")
public class PypiArtifactController extends BaseArtifactController {

    private static final Set<String> VALID_ACTIONS = Sets.newHashSet("file_upload");

    private static final Set<String> VALID_FILE_TYPES = Sets.newHashSet("sdist", "bdist_wheel");

    @Inject
    private PypiService pypiService;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    private PypiBrowsePackageHtmlResponseBuilder pypiBrowsePackageHtmlResponseBuilder;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }


    @ApiOperation(value = "This end point will be used to upload/deploy python package.")
    @ApiResponses(value = {@ApiResponse(code = HttpURLConnection.HTTP_OK, message = "python package was deployed successfully."),
            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing request."),
            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "/{storageId}/{repositoryId}", method = RequestMethod.POST, consumes = MediaType.MULTIPART_FORM_DATA)
    public ResponseEntity<String> uploadPackage(
            @RepoMapping Repository repository,
            @RequestParam(name = "comment", required = false) String comment,
            @RequestParam(name = "metadata_version", required = true) String metadataVersion,
            @RequestParam(name = "filetype", required = true) String filetype,
            @RequestParam(name = "protcol_version", required = false) String protcolVersion,
            @RequestParam(name = "author", required = false) String author,
            @RequestParam(name = "home_page", required = false) String homePage,
            @RequestParam(name = "download_url", required = false) String downloadUrl,
            @RequestParam(name = "platform", required = false) String platform,
            @RequestParam(name = "version", required = false) String version,
            @RequestParam(name = "description", required = false) String description,
            @RequestParam(name = "md5_digest", required = false) String md5Digest,
            @RequestParam(name = ":action", required = true) String action,
            @RequestParam(name = "name", required = true) String name,
            @RequestParam(name = "license", required = false) String license,
            @RequestParam(name = "pyversion", required = false) String pyversion,
            @RequestParam(name = "summary", required = false) String summary,
            @RequestParam(name = "author_email", required = false) String authorEmail,
            @RequestParam(name = "content", required = true) MultipartFile file,
            HttpServletRequest request) {
        String repositoryId = repository.getId();
        logger.info("python package upload request for storageId -> [{}] , repositoryId -> [{}]",
                repository.getStorage().getId(),
                repositoryId);

        try {
            if (!isValidAction(action)) {
                return ResponseEntity.status(HttpURLConnection.HTTP_BAD_REQUEST)
                        .body("Invalid value for \":action\" parameter. Valid action values are "
                                + VALID_ACTIONS);
            }
            PypiArtifactMetadata pypiArtifactMetadata = new PypiArtifactMetadata().withAction(action)
                    .withAuthor(author)
                    .withAuthorEmail(authorEmail)
                    .withComment(comment)
                    .withDescription(description)
                    .withDownloadUrl(downloadUrl)
                    .withFileType(filetype)
                    .withHomePage(homePage)
                    .withLicense(license)
                    .withMd5Digest(md5Digest)
                    .withMetdataVersion(metadataVersion)
                    .withName(name)
                    .withPlatform(platform)
                    .withProtcolVersion(protcolVersion)
                    .withPyVersion(pyversion)
                    .withSummary(summary)
                    .withVersion(version);

            return validateAndUploadPackage(pypiArtifactMetadata, file, repository.getStorage().getId(),
                    repositoryId);
        } catch (Exception e) {
            logger.error("Failed to process pypi upload request for storageId -> [{}] , repositoryId -> [{}]",
                    repository.getStorage().getId(), repository.getId(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }

    @ApiOperation(value = "This Endpoint will be used for download/install pypi package")
    @ApiResponses(value = {@ApiResponse(code = HttpURLConnection.HTTP_OK, message = "Success"),
            @ApiResponse(code = HttpURLConnection.HTTP_SEE_OTHER, message = "See Other Location"),
            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Request Url Not Found"),
            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing download request."),
            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "/{storageId}/{repositoryId}/{packageName}", method = RequestMethod.GET)
    public ResponseEntity<String> downloadPackage(@RepoMapping Repository repository,
                                                  @PathVariable(name = "packageName") String packageName,
                                                  HttpServletRequest request) {

        logger.info("install package request for storageId -> [{}] , repositoryId -> [{}], packageName -> [{}]",
                repository.getStorage().getId(),
                repository.getId(), packageName);

        final Map<String, String> uriVariables = new HashMap<String, String>();
        uriVariables.put("storageId", repository.getStorage().getId());
        uriVariables.put("repositoryId", repository.getId());
        uriVariables.put("packageName", packageName);

        final URI location = ServletUriComponentsBuilder
                .fromCurrentServletMapping()
                .path("/storages/{storageId}/{repositoryId}/simple/{packageName}/")
                .build()
                .expand(uriVariables)
                .toUri();

        final HttpHeaders headers = new HttpHeaders();
        headers.setLocation(location);
        return ResponseEntity.status(HttpStatus.SEE_OTHER)
                .headers(headers)
                .body(HttpStatus.SEE_OTHER.getReasonPhrase());
    }

    @ApiOperation(value = "This Endpoint will be used to retreive pypi artifact/.whl file")
    @ApiResponses(value = {@ApiResponse(code = HttpURLConnection.HTTP_OK, message = "Success"),
            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Request Url Not Found"),
            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing download request."),
            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "/{storageId}/{repositoryId}/packages/{artifactName:.+}", method = RequestMethod.GET)
    public void downloadPackage(@RepoMapping Repository repository,
                                @PathVariable(name = "artifactName") String artifactName,
                                HttpServletRequest request,
                                HttpServletResponse response,
                                @RequestHeader HttpHeaders headers)
            throws Exception {

        logger.info("Download package request for storageId -> [{}] , repositoryId -> [{}], artifactName -> [{}]",
                repository.getStorage().getId(),
                repository.getId(), artifactName);

        PypiCoordinates coordinates;
        try {
            coordinates = PypiCoordinates.parse(artifactName);
        } catch (IllegalArgumentException e) {
            logger.error("Invalid package name - {}", e.getMessage());
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return;
        }
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(
                repository.getStorage().getId(),
                repository.getId(),
                coordinates.buildPath());
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, headers, repositoryPath);
    }

    private ArtifactIdGroup getArtifactIdGroup(Repository repository, PypiCoordinates coordinates) {
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), coordinates.getId());
        artifactIdGroup = artifactComponent.getArtifactIdGroup(artifactIdGroup.getUuid());
        if (Objects.isNull(artifactIdGroup)) {
            artifactIdGroup = new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), coordinates.getId().toLowerCase());
            artifactIdGroup = artifactComponent.getArtifactIdGroup(artifactIdGroup.getUuid());
        }
        if (Objects.isNull(artifactIdGroup)) {
            String id = coordinates.getId();
            if (id.contains("-")) {
                id = id.replace("-", "_");
            } else if (id.contains("_")) {
                id = id.replace("_", "-");
            }
            artifactIdGroup = new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), id);
            artifactIdGroup = artifactComponent.getArtifactIdGroup(artifactIdGroup.getUuid());
        }
        return artifactIdGroup;
    }

    @ApiOperation(value = "This Endpoint will be used to retreive all the versions of packages present in artifactory.")
    @ApiResponses(value = {@ApiResponse(code = HttpURLConnection.HTTP_OK, message = "Success"),
            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Request Url Not Found"),
            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing download request."),
            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "/{storageId}/{repositoryId}/simple/{packageName}", method = RequestMethod.GET, produces = MediaType.TEXT_HTML)
    public ResponseEntity<String> browsePackage(@RepoMapping Repository repository,
                                                @PathVariable(name = "packageName") String packageName,
                                                HttpServletRequest request,
                                                HttpServletResponse response,
                                                @RequestHeader HttpHeaders headers)
            throws Exception {
        if (!request.getRequestURI().endsWith("/")) {
            final Map<String, String> uriVariables = new HashMap<String, String>();
            uriVariables.put("storageId", repository.getStorage().getId());
            uriVariables.put("repositoryId", repository.getId());
            uriVariables.put("packageName", packageName);

            final URI location = ServletUriComponentsBuilder
                    .fromCurrentServletMapping()
                    .path("/storages/{storageId}/{repositoryId}/simple/{packageName}/")
                    .build()
                    .expand(uriVariables)
                    .toUri();
            headers.setLocation(location);
            return ResponseEntity.status(HttpStatus.SEE_OTHER)
                    .headers(headers)
                    .body(HttpStatus.SEE_OTHER.getReasonPhrase());
        }
        final String packageNameToDownload = PypiPackageNameConverter.escapeSpecialCharacters(packageName);

        logger.info("Get package path request for storageId -> [{}] , repositoryId -> [{}], packageName -> [{}]",
                repository.getStorage().getId(),
                repository.getId(), packageNameToDownload);
        String html = pypiService.packages(repository, packageName, packageName);
        if (StringUtils.isBlank(html)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(pypiBrowsePackageHtmlResponseBuilder.nouFound());
        }
        return ResponseEntity.status(HttpStatus.OK).body(html);
    }

    private ResponseEntity<String> validateAndUploadPackage(PypiArtifactMetadata pypiArtifactMetadata,
                                                            MultipartFile file,
                                                            String storageId,
                                                            String repositoryId)
            throws IOException,
            ProviderImplementationException,
            ArtifactCoordinatesValidationException {
        if (!isValidFileType(pypiArtifactMetadata.getFileType())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body("Invalid value for \"filetype\" parameter.Valid values are " + VALID_FILE_TYPES);
        }

        PypiCoordinates coordinates = PypiCoordinates.parse(file.getOriginalFilename());

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId,
                repositoryId,
                coordinates.buildPath());
        try (InputStream is = file.getInputStream()) {
            artifactManagementService.validateAndStore(repositoryPath, is);
        } catch (IOException e) {
            logger.error(e.getMessage(), e);
            throw new IOException(e);
        }


        return ResponseEntity.status(HttpStatus.OK).body("The artifact was deployed successfully.");
    }

    private boolean isValidFileType(String fileType) {
        if (StringUtils.isEmpty(fileType)) {
            return false;
        }
        return VALID_FILE_TYPES.contains(fileType);
    }

    private boolean isValidAction(String action) {
        if (StringUtils.isEmpty(action)) {
            return false;
        }
        return VALID_ACTIONS.contains(action);
    }

}
