package com.folib.controllers.layout.maven;

import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.controllers.BaseArtifactController;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.repository.Repository;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;


import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;

import static org.springframework.http.HttpStatus.NOT_FOUND;

/**
 * REST API for all artifact-related processes.
 * <p>
 * Thanks to custom URL processing any path variable like '{artifactPath:.+}' will be processed as '**'.
 *
 * @author Martin Todorov
 * @author
 * @author veadan
 * @author @author veadan
 */
@RestController
@LayoutReqMapping(MavenCoordinates.LAYOUT_NAME)
//@RequestMapping(
//        headers = "user-agent=Maven/*")
@Api(description = "maven坐标控制器", tags = "maven坐标控制器")

public class MavenArtifactController
        extends BaseArtifactController {

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(
            @RequestHeader HttpHeaders httpHeaders,
            @PathVariable String artifactPath,
            @PathVariable String storageId,
            @PathVariable String repositoryId,
            HttpServletRequest request,
            HttpServletResponse response)
            throws Exception {
        long startTime = System.currentTimeMillis();
        logger.info("Requested /{}/{}/{}", storageId, repositoryId, artifactPath);
//        artifactPath = correctIndexPathIfNecessary(repository, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        logger.debug("Requested /{}/{}/{} endTime {} .", storageId, repositoryId, artifactPath, System.currentTimeMillis() - startTime);
    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity upload(@RepoMapping Repository repository,
                                 @PathVariable String artifactPath,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try (InputStream is = request.getInputStream()) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, is);

            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (ArtifactStorageException e) {
            logger.error("Unable to copy artifact due to ArtifactStorageException", e);
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(e.getMessage());
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Copies a path from one repository to another.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The path was copied successfully."),
            @ApiResponse(code = 400, message = "Bad request."),
            @ApiResponse(code = 404, message = "The source/destination storageId/repositoryId/path does not exist!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_COPY')")
    @PostMapping(value = "/copy/{path:.+}")
    public ResponseEntity copy(
            @RepoMapping(storageVariableName = "srcStorageId", repositoryVariableName = "srcRepositoryId")
                    Repository srcRepository,
            @RepoMapping(storageVariableName = "destStorageId", repositoryVariableName = "destRepositoryId")
                    Repository destRepository,
            @PathVariable String path) {
        final String srcStorageId = srcRepository.getStorage().getId();
        final String srcRepositoryId = srcRepository.getId();
        final String destStorageId = destRepository.getStorage().getId();
        final String destRepositoryId = destRepository.getId();

        logger.info("Copying {} from {}:{} to {}:{}...", path, srcStorageId, srcRepositoryId, destStorageId,
                destRepositoryId);

        try {
            final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, path);
            if (!Files.exists(srcRepositoryPath)) {
                return ResponseEntity.status(NOT_FOUND)
                        .body("The source path does not exist!");
            }

            RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, path);
            RepositoryPath destPath = repositoryPathResolver.resolve(destRepository, path);

            artifactManagementService.copy(srcPath, destPath);
        } catch (ArtifactStorageException e) {
            logger.error("Unable to copy artifact due to ArtifactStorageException", e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(e.getMessage());
        } catch (Exception e) {
            logger.error("Unable to copy artifact", e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }

        return ResponseEntity.ok("The path was copied successfully.");
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

    private String correctIndexPathIfNecessary(final Repository repository,
                                               final String requestedPath) {
        return new MavenRepositoryIndexPathTransformer(repository).apply(requestedPath);
    }

}
