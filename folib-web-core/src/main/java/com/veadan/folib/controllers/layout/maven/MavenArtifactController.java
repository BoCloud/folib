package com.veadan.folib.controllers.layout.maven;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.file.Files;
import java.util.concurrent.RejectedExecutionException;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
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
 *
 * @see {@linkplain http://docs.spring.io/spring/docs/current/spring-framework-reference/html/mvc.html#mvc-config-path-matching}
 */
@RestController
@LayoutRequestMapping(MavenArtifactCoordinates.LAYOUT_NAME)
public class MavenArtifactController
        extends BaseArtifactController
{

    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess()
    {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
                            @ApiResponse(code = 404, message = "Requested path not found."),
                            @ApiResponse(code = 500, message = "Server error."),
                            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = { "/{storageId}/{repositoryId}/{artifactPath:.+}" }, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String artifactPath,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.debug("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);

        artifactPath = correctIndexPathIfNecessary(repository, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
//        boolean flag = !SafeLevelEnum.SCAN_COMPLETE.getLevel().equals(artifact.getSafeLevel()) || artifact.getVulnerabilitiesCount() > 0;
//        if (flag) {
//            throw new RejectedExecutionException("该制品存在漏洞，禁止下载！！！");
//        }
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The artifact was deployed successfully."),
                            @ApiResponse(code = 400, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity upload(@RepositoryMapping Repository repository,
                                 @PathVariable String artifactPath,
                                 HttpServletRequest request)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try
        {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, request.getInputStream());

            return ResponseEntity.ok("The artifact was deployed successfully.");
        }
        catch (Exception e)
        {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Copies a path from one repository to another.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The path was copied successfully."),
                            @ApiResponse(code = 400, message = "Bad request."),
                            @ApiResponse(code = 404, message = "The source/destination storageId/repositoryId/path does not exist!") })
    @PreAuthorize("hasAuthority('ARTIFACTS_COPY')")
    @PostMapping(value = "/copy/{path:.+}")
    public ResponseEntity copy(
            @RepositoryMapping(storageVariableName = "srcStorageId", repositoryVariableName = "srcRepositoryId")
                    Repository srcRepository,
            @RepositoryMapping(storageVariableName = "destStorageId", repositoryVariableName = "destRepositoryId")
                    Repository destRepository,
            @PathVariable String path)
    {
        final String srcStorageId = srcRepository.getStorage().getId();
        final String srcRepositoryId = srcRepository.getId();
        final String destStorageId = destRepository.getStorage().getId();
        final String destRepositoryId = destRepository.getId();

        logger.debug("Copying {} from {}:{} to {}:{}...", path, srcStorageId, srcRepositoryId, destStorageId,
                     destRepositoryId);

        try
        {
            final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, path);
            if (!Files.exists(srcRepositoryPath))
            {
                return ResponseEntity.status(NOT_FOUND)
                                     .body("The source path does not exist!");
            }

            RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, path);
            RepositoryPath destPath = repositoryPathResolver.resolve(destRepository, path);

            artifactManagementService.copy(srcPath, destPath);
        }
        catch (ArtifactStorageException e)
        {
            logger.error("Unable to copy artifact due to ArtifactStorageException", e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(e.getMessage());
        }
        catch (Exception e)
        {
            logger.error("Unable to copy artifact", e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                                 .body(e.getMessage());
        }

        return ResponseEntity.ok("The path was copied successfully.");
    }

    @ApiOperation(value = "Deletes a path from a repository.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The artifact was deleted."),
                            @ApiResponse(code = 400, message = "Bad request."),
                            @ApiResponse(code = 404, message = "The specified storageId/repositoryId/path does not exist!") })
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity delete(@RepositoryMapping Repository repository,
                                 @ApiParam(value = "Whether to use force delete")
                                 @RequestParam(defaultValue = "false",
                                               name = "force",
                                               required = false) boolean force,
                                 @PathVariable String artifactPath)
            throws IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Deleting {}:{}/{}...", storageId, repositoryId, artifactPath);

        try
        {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath))
            {
                return ResponseEntity.status(NOT_FOUND)
                                     .body("The specified path does not exist!");
            }

            artifactManagementService.delete(repositoryPath, force);
        }
        catch (ArtifactStorageException e)
        {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(e.getMessage());
        }

        return ResponseEntity.ok("The artifact was deleted.");
    }

    private String correctIndexPathIfNecessary(final Repository repository,
                                               final String requestedPath)
    {
        return new MavenRepositoryIndexPathTransformer(repository).apply(requestedPath);
    }

}
