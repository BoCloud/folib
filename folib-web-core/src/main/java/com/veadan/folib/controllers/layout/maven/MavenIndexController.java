package com.veadan.folib.controllers.layout.maven;

import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.providers.layout.IndexingDisabledException;
import com.veadan.folib.providers.layout.MavenFileSystem;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;

import javax.inject.Inject;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import static com.veadan.folib.config.Maven2LayoutProviderConfig.FILE_SYSTEM_ALIAS;

/**
 * @author Kate Novik
 * @author carlspring
 * @author Przemyslaw Fusik
 */
@RestController
@RequestMapping("/api/maven/index")
public class MavenIndexController
        extends BaseController
{

    @Inject
    @Qualifier(FILE_SYSTEM_ALIAS)
    private LayoutFileSystemFactory layoutFileSystemFactory;

    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_INDEXES')")
    @PostMapping(value = "/{storageId}/{repositoryId}", produces = { MediaType.TEXT_PLAIN_VALUE,
                                                                     MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity rebuildIndex(@RepositoryMapping Repository repository)
    {
        try
        {
            LayoutFileSystem layoutFileSystem = layoutFileSystemFactory.create(repository);
            RepositoryPath indexPath = ((MavenFileSystem) layoutFileSystem).rebuildIndex(repository);

            return ResponseEntity.ok(String.format("Index was regenerated in [%s].", indexPath));
        }
        catch (IndexingDisabledException e)
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Indexing is disabled on this repository.");
        }
        catch (IOException e)
        {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                                 .body(e.getMessage());
        }
    }
}
