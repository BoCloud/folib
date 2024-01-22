package com.veadan.folib.event.artifact;

import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;

/**
 * @author veadan
 */
@Component
public class MavenArtifactDeletedEventListener
        extends BaseMavenArtifactEventListener {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType()) {
            return;
        }
        try {
            RepositoryPath repositoryPath = event.getPath();
            mavenMetadataGroupRepositoryComponent.cleanupGroupsContaining(repositoryPath);
            String storageId = repository.getStorage().getId();
            String repositoryId = repository.getId();

            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (Files.exists(repositoryPath.getParent()) && Files.isSameFile(repositoryPath.getRoot(), repositoryPath.getParent())) {
                return;
            }
            artifactMetadataService.rebuildMetadata(storageId, repositoryId, artifactPath);
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
    }
}
