package com.folib.event;

import com.folib.enums.NpmIndexTypeEnum;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.indexer.NpmPackageMetadataIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.NpmLayoutProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * @author veadan
 */
@Component
public class NpmArtifactEventListener {

    @Inject
    private NpmPackageMetadataIndexer npmPackageMetadataIndexer;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!NpmLayoutProvider.ALIAS.equals(repository.getLayout()) || !RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType()) {
            return;
        }
        try {
            switch (event.getType()) {
                case 10:
                case 22:
                case 23:
                    npmPackageMetadataIndexer.indexAsSystem(event.getPath(), NpmIndexTypeEnum.REINDEX);
                    break;
                case 16:
                    npmPackageMetadataIndexer.indexAsSystem(event.getPath(), NpmIndexTypeEnum.DELETE);
                    break;
                default:
                    ;
            }
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
    }

    Repository getRepository(final ArtifactEvent<RepositoryPath> event) {
        return event.getPath().getFileSystem().getRepository();
    }
}
