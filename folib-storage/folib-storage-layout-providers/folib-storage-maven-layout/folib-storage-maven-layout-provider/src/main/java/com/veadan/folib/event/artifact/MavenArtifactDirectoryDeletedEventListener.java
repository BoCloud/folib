package com.veadan.folib.event.artifact;

import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.File;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;

/**
 * @author veadan
 */
@Component
public class MavenArtifactDirectoryDeletedEventListener
        extends BaseMavenArtifactEventListener {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType()) {
            return;
        }

        try {
            String storageId = repository.getStorage().getId();
            String repositoryId = repository.getId();
            String path = event.getPath().toUri().getPath();
            String format = "%s/%s/";
            if (path.startsWith(File.separator)) {
                format = "/%s/%s/";
            }
            String artifactPath = path.replace(String.format(format, storageId, repositoryId), "");
            if (StringUtils.isNotBlank(artifactPath)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                if (Files.isSameFile(repositoryPath.getRoot(), repositoryPath.getParent())) {
                    return;
                }
                artifactMetadataService.rebuildMetadata(storageId, repositoryId, artifactPath);
            }
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
    }
}
