package com.veadan.folib.controllers.layout.pypi;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.ArtifactGroup;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.pypi.PypiSearchRequest;
import com.veadan.folib.pypi.PypiSearchResult;
import com.veadan.folib.repository.PypiRepositoryFeatures;
import com.veadan.folib.services.ArtifactCoordinatesService;
import com.veadan.folib.storage.metadata.pypi.PypiArtifactMetadata;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.utils.PypiPackageNameConverter;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.Path;
import java.util.*;

/**
 * Rest End Points for Pypi Artifacts requests.
 * This controller will be Entry point for various pip commands.
 *
 * @author ankit.tomar
 *
 */
@RestController
@LayoutRequestMapping(PypiArtifactCoordinates.LAYOUT_NAME)
public class PypiArtifactController extends BaseArtifactController
{

    private static final Set<String> VALID_ACTIONS = Sets.newHashSet("file_upload");

    private static final Set<String> VALID_FILE_TYPES = Sets.newHashSet("sdist", "bdist_wheel");

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private PypiBrowsePackageHtmlResponseBuilder htmlResponseBuilder;

    @Inject
    private PypiRepositoryFeatures.PypiSearchPackagesEventListener pypiSearchPackagesEventListener;

    @Inject
    private ArtifactCoordinatesService artifactCoordinatesService;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @ApiOperation(value = "This end point will be used to upload/deploy python package.")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "python package was deployed successfully."),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing request."),
                            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "/{storageId}/{repositoryId}", method = RequestMethod.POST, consumes = MediaType.MULTIPART_FORM_DATA)
    public ResponseEntity<String> uploadPackage(
                                                @RepositoryMapping Repository repository,
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
                                                HttpServletRequest request)
    {

        logger.info("python package upload request for storageId -> [{}] , repositoryId -> [{}]",
                    repository.getStorage().getId(),
                    repository.getId());

        try
        {
            if (!isValidAction(action))
            {
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
                                            repository.getId());
        }
        catch (Exception e)
        {
            logger.error("Failed to process pypi upload request for storageId -> [{}] , repositoryId -> [{}]",
                         repository.getStorage().getId(), repository.getId(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }

    @ApiOperation(value = "This Endpoint will be used for download/install pypi package")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "Success"),
                            @ApiResponse(code = HttpURLConnection.HTTP_SEE_OTHER, message = "See Other Location"),
                            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Request Url Not Found"),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing download request."),
                            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "/{storageId}/{repositoryId}/{packageName}", method = RequestMethod.GET)
    public ResponseEntity<String> downloadPackage(@RepositoryMapping Repository repository,
                                                  @PathVariable(name = "packageName") String packageName,
                                                  HttpServletRequest request)
    {

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
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "Success"),
                            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Request Url Not Found"),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing download request."),
                            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "/{storageId}/{repositoryId}/packages/{artifactName}", method = RequestMethod.GET)
    public void downloadPackage(@RepositoryMapping Repository repository,
                                @PathVariable(name = "artifactName") String artifactName,
                                HttpServletRequest request,
                                HttpServletResponse response,
                                @RequestHeader HttpHeaders headers)
        throws Exception
    {

        logger.info("Download package request for storageId -> [{}] , repositoryId -> [{}], artifactName -> [{}]",
                    repository.getStorage().getId(),
                    repository.getId(), artifactName);

        PypiArtifactCoordinates coordinates;
        try
        {
            coordinates = PypiArtifactCoordinates.parse(artifactName);
        }
        catch (IllegalArgumentException e)
        {
            logger.error("Invalid package name - {}", e.getMessage());
            response.setStatus(HttpStatus.BAD_REQUEST.value());
            return;
        }

        String remoteRepositoryUrl;
        RemoteRepository remoteRepository;
        String targetUrl = null;
        if (repository.isProxyRepository() && Objects.nonNull(remoteRepository = repository.getRemoteRepository())
                && Objects.nonNull(remoteRepositoryUrl = remoteRepository.getUrl())) {
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), coordinates.getId());
            artifactIdGroup = artifactComponent.getArtifactIdGroup(artifactIdGroup.getUuid());
            if (Objects.isNull(artifactIdGroup)) {
                artifactIdGroup = new ArtifactIdGroupEntity(repository.getStorage().getId(), repository.getId(), coordinates.getId().toLowerCase());
                artifactIdGroup = artifactComponent.getArtifactIdGroup(artifactIdGroup.getUuid());
            }
            if (Objects.nonNull(artifactIdGroup) && StringUtils.hasText(artifactIdGroup.getMetadata())) {
                List<PypiSearchResult> pypiSearchRequestList = JSONObject.parseArray(artifactIdGroup.getMetadata(), PypiSearchResult.class);
                Optional<PypiSearchResult> optionalPypiSearchResult = pypiSearchRequestList.stream().filter(item -> item.getArtifactName().equals(artifactName)).findFirst();
                if (optionalPypiSearchResult.isPresent()) {
                    targetUrl = String.format("%s%s", remoteRepositoryUrl, optionalPypiSearchResult.get().getArtifactUrl());
                }
            }
        }
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(
                                                                              repository.getStorage().getId(),
                                                                              repository.getId(),
                                                                              targetUrl,
                                                                              coordinates.buildPath());
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, headers, repositoryPath);
    }

    @ApiOperation(value = "This Endpoint will be used to retreive all the versions of packages present in artifactory.")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "Success"),
                            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Request Url Not Found"),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred while executing download request."),
                            @ApiResponse(code = HttpURLConnection.HTTP_UNAVAILABLE, message = "Service Unavailable.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(path = "/{storageId}/{repositoryId}/simple/{packageName}", method = RequestMethod.GET, produces = MediaType.TEXT_HTML)
    public ResponseEntity<String> browsePackage(@RepositoryMapping Repository repository,
                                                @PathVariable(name = "packageName") String packageName,
                                                HttpServletRequest request,
                                                HttpServletResponse response,
                                                @RequestHeader HttpHeaders headers)
        throws Exception
    {

        final String packageNameToDownload = PypiPackageNameConverter.escapeSpecialCharacters(packageName);

        logger.info("Get package path request for storageId -> [{}] , repositoryId -> [{}], packageName -> [{}]",
                    repository.getStorage().getId(),
                    repository.getId(), packageNameToDownload);
        String html = artifactComponent.getPypiArtifactIdGroupCache(repository, new PypiSearchRequest(packageName));
        return ResponseEntity.status(HttpStatus.OK).body(html);
    }

    private ResponseEntity<String> validateAndUploadPackage(PypiArtifactMetadata pypiArtifactMetadata,
                                                            MultipartFile file,
                                                            String storageId,
                                                            String repositoryId)
        throws IOException,
        ProviderImplementationException,
            ArtifactCoordinatesValidationException
    {
        if (!isValidFileType(pypiArtifactMetadata.getFileType()))
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body("Invalid value for \"filetype\" parameter.Valid values are " + VALID_FILE_TYPES);
        }

        PypiArtifactCoordinates coordinates = PypiArtifactCoordinates.parse(file.getOriginalFilename());

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId,
                                                                       repositoryId,
                                                                       coordinates.buildPath());
        artifactManagementService.validateAndStore(repositoryPath, file.getInputStream());

        return ResponseEntity.status(HttpStatus.OK).body("The artifact was deployed successfully.");
    }

    private boolean isValidFileType(String fileType)
    {
        if (StringUtils.isEmpty(fileType))
        {
            return false;
        }
        return VALID_FILE_TYPES.contains(fileType);
    }

    private boolean isValidAction(String action)
    {
        if (StringUtils.isEmpty(action))
        {
            return false;
        }
        return VALID_ACTIONS.contains(action);
    }

}
