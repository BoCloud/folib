package com.veadan.folib.controllers.layout.pub;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.constants.PubConstants;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.PubPackageMetadata;
import com.veadan.folib.domain.PubPackageVersionMetadata;
import com.veadan.folib.domain.PubUpload;
import com.veadan.folib.domain.Pubspec;
import com.veadan.folib.domain.common.StandardError;
import com.veadan.folib.domain.common.StandardResponse;
import com.veadan.folib.enums.PubIndexTypeEnum;
import com.veadan.folib.indexer.PubMetadataExtractor;
import com.veadan.folib.indexer.PubPackageMetadataIndexer;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.PubService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
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
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import java.io.BufferedInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;


/**
 * This Controller used to handle pub requests.
 *
 * @author leipenghui
 */
@RestController
@LayoutRequestMapping(PubArtifactCoordinates.LAYOUT_NAME)
@Api(description = "pub仓库控制器", tags = "pub仓库控制器")
public class PubArtifactController
        extends BaseArtifactController {

    private static final String PACKAGES_ENDPOINT = "/api/packages/";

    @Inject
    @DatabaseUserService.Database
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
    public ResponseEntity inspectVersion(@RepositoryMapping Repository repository, @PathVariable(name = "storageId") String storageId, @PathVariable(name = "repositoryId") String repositoryId,
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
    public ResponseEntity packages(@RepositoryMapping Repository repository,
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
    public void download(@RepositoryMapping Repository repository,
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
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity getUrlDeployment(@RepositoryMapping Repository repository,
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
    public ResponseEntity deploy(@RepositoryMapping Repository repository,
                                 @PathVariable(name = "storageId") String storageId,
                                 @PathVariable(name = "repositoryId") String repositoryId,
                                 HttpServletRequest request,
                                 @RequestParam("file") MultipartFile file,
                                 HttpServletResponse response) throws Exception {
        PubMetadataExtractor extractor = new PubMetadataExtractor();
        Pair<Pubspec, Path> pubspecPathPair = extractor.extractPubSpec(file.getInputStream());
        try (InputStream bufferedInputStream = new BufferedInputStream(Files.newInputStream(pubspecPathPair.getValue1()))) {
            Pubspec pubspec = pubspecPathPair.getValue0();
            PubArtifactCoordinates pubArtifactCoordinates = PubArtifactCoordinates.of(pubspec.getName(), pubspec.getVersion(), PubArtifactCoordinates.PUB_EXTENSION);
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
        response.setHeader("Location", getRepositoryBaseUrl(repository) + "/finalizeDeployment");
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

    @GetMapping(path = "{storageId}/{repositoryId}/finalizeDeployment")
    @ApiOperation(value = "Finalize the deploy process.", nickname = "finalizeDeployment")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity finalizeDeployment(@PathVariable(name = "storageId") String storageId,
                                             @PathVariable(name = "repositoryId") String repositoryId, HttpServletResponse response) {
        response.setHeader("Content-Type", PubConstants.CONTENT_TYPE);
        return ResponseEntity.ok(PubConstants.GET_FINALIZE_DEPLOYMENT_RESULT);
    }
}
