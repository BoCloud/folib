package com.veadan.folib.event;

import com.veadan.folib.artifact.coordinates.ConanArtifactIndex;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.ConanLayoutProvider;
import com.veadan.folib.services.ArtifactIndexService;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.Objects;

/**
 * @author veadan
 */
@Component
public class ConanArtifactEventListener {

    @Inject
    private ArtifactIndexService artifactIndexService;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!ConanLayoutProvider.ALIAS.equals(repository.getLayout())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType()) {
            return;
        }
        try {
            RepositoryPath repositoryPath = event.getPath();
            String storageId = repository.getStorage().getId();
            String repositoryId = repository.getId();
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            ConanArtifactIndex conanArtifactIndex = ConanArtifactIndex.parse(artifactPath);
            if (Objects.isNull(conanArtifactIndex) || StringUtils.isBlank(conanArtifactIndex.getIndexRelativizePath())) {
                return;
            }
            artifactIndexService.rebuildIndex(storageId, repositoryId, conanArtifactIndex.getIndexRelativizePath());
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
    }

    Repository getRepository(final ArtifactEvent<RepositoryPath> event) {
        return event.getPath().getFileSystem().getRepository();
    }
}
