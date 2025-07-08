package com.folib.controllers.layout.maven;

import com.folib.providers.io.LayoutFileSystem;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.RepositoryPath;
import com.folib.controllers.BaseController;
import com.folib.providers.layout.IndexingDisabledException;
import com.folib.providers.layout.MavenFileSystem;
import com.folib.storage.repository.Repository;
import com.folib.web.RepositoryMapping;

import javax.inject.Inject;
import java.io.IOException;

import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import static com.folib.config.Maven2LayoutProviderConfig.FILE_SYSTEM_ALIAS;

/**
 * @author Kate Novik
 * @author Veadan
 * @author veadan
 */
@RestController
@RequestMapping("/api/maven/index")
@Api(description = "maven索引控制器",tags = "maven索引控制器")
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
