package com.veadan.folib.controllers;

import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

/**
 * @author Martin Todorov
 * @author Pablo Tirado
 */
@Controller
@RequestMapping("/api/trash")
@Api(value = "/api/trash")
public class TrashController
        extends BaseController
{

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @ApiOperation(value = "Used to delete the trash for a specified repository.")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "The trash for ${storageId}:${repositoryId}' was removed successfully."),
                            @ApiResponse(code = 400,
                                         message = "Could not delete the trash for a specified storageId/repositoryId."),
                            @ApiResponse(code = 404,
                                         message = "The specified (storageId/repositoryId) does not exist!") })
    @PreAuthorize("hasAuthority('MANAGEMENT_DELETE_TRASH')")
    @DeleteMapping(value = "{storageId}/{repositoryId}",
                   produces = { MediaType.TEXT_PLAIN_VALUE,
                                MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity delete(@RepositoryMapping Repository repository,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try
        {
            repositoryManagementService.deleteTrash(storageId, repositoryId);

            logger.debug("Deleted trash for repository {}.", repositoryId);
        }
        catch (ArtifactStorageException e)
        {
            String message = "Could not delete the trash for a specified storageId/repositoryId.";
            logger.error(message, e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody(message, accept));
        }

        String message = "The trash for '" + storageId + ":" + repositoryId + "' was removed successfully.";
        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "Used to delete the trash for all repositories.")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "The trash for all repositories was successfully removed."),
                            @ApiResponse(code = 400,
                                         message = "Could not delete the trash for all repositories.") })
    @PreAuthorize("hasAuthority('MANAGEMENT_DELETE_ALL_TRASHES')")
    @DeleteMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                                MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity delete(@RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws IOException
    {
        try
        {
            repositoryManagementService.deleteTrash();

            logger.debug("Deleted trash for all repositories.");
        }
        catch (ArtifactStorageException e)
        {
            String message = "Could not delete the trash for all repositories.";
            logger.error(message, e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody(message, accept));
        }

        String message = "The trash for all repositories was successfully removed.";
        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "Used to undelete the trash for a path under a specified repository.")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "The trash for '${storageId}:${repositoryId}' was restored successfully."),
                            @ApiResponse(code = 400,
                                         message = "Could not restore the trash for the specified repository."),
                            @ApiResponse(code = 404,
                                         message = "The specified (storageId/repositoryId/path) does not exist!") })
    @PreAuthorize("hasAuthority('MANAGEMENT_UNDELETE_TRASH')")
    @PostMapping("{storageId}/{repositoryId}/{path:.+}")
    public ResponseEntity undelete(@RepositoryMapping Repository repository,
                                   @PathVariable String path,
                                   @RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, path);
        RepositoryPath trashPath = RepositoryFiles.trash(repositoryPath);
        if (!Files.exists(trashPath))
        {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                                 .body(getResponseEntityBody("The specified path does not exist!", accept));
        }

        try
        {
            repositoryManagementService.undelete(repositoryPath);

            logger.debug("Undeleted trash for path {} under repository {}:{}.", path, storageId, repositoryId);
        }
        catch (ArtifactStorageException e)
        {
            String message = "Could not restore the trash for the specified repository.";

            logger.error(message, e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody(message, accept));
        }

        String message = "The trash for '" + storageId + ":" + repositoryId + "' was restored successfully.";
        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "Used to undelete the trash for a specified repository.")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "The trash for '${storageId}:${repositoryId}' was restored successfully."),
                            @ApiResponse(code = 400,
                                         message = "Could not restore the trash for a specified repository."),
                            @ApiResponse(code = 404,
                                         message = "The specified (storageId/repositoryId) does not exist!") })
    @PreAuthorize("hasAuthority('MANAGEMENT_UNDELETE_TRASH')")
    @PutMapping(value = "{storageId}/{repositoryId}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity undelete(@RepositoryMapping Repository repository,
                                   @RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws Exception
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try
        {
            repositoryManagementService.undeleteTrash(storageId, repositoryId);

            logger.debug("Undeleted trash for repository {}.", repositoryId);
        }
        catch (ArtifactStorageException e)
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody("Could not restore the trash for a specified repository.", accept));
        }

        return ResponseEntity.ok(getResponseEntityBody("The trash in '" + storageId + ":" + repositoryId + "' " +
                                                       "has been restored successfully.", accept));
    }

    @ApiOperation(value = "Used to undelete the trash for all repositories.")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "The trash for all repositories was successfully restored."),
                            @ApiResponse(code = 400,
                                         message = "Could not restore the trash for all repositories.") })
    @PreAuthorize("hasAuthority('MANAGEMENT_UNDELETE_ALL_TRASHES')")
    @PostMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                              MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity undelete(@RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws Exception
    {
        try
        {
            repositoryManagementService.undeleteTrash();

            logger.debug("Undeleted trash for all repositories.");
        }
        catch (ArtifactStorageException e)
        {
            String message = "Could not restore the trash for all repositories.";

            logger.error(message, e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody(message, accept));
        }

        return ResponseEntity.ok(getResponseEntityBody("The trash for all repositories was successfully restored.",
                                                       accept));
    }

}
