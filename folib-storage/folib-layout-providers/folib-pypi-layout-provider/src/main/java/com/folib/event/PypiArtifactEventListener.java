package com.folib.event;

import com.folib.enums.PypiIndexTypeEnum;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.indexer.PypiPackageMetadataIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.PypiLayoutProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * @author veadan
 */
@Component
public class PypiArtifactEventListener {

    @Inject
    private PypiPackageMetadataIndexer pypiPackageMetadataIndexer;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!PypiLayoutProvider.ALIAS.equals(repository.getLayout()) || !RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType()) {
            return;
        }
        try {
            switch (event.getType()) {
                case 10:
                case 22:
                    pypiPackageMetadataIndexer.indexAsSystem(event.getPath(), PypiIndexTypeEnum.ADD);
                    break;
                case 23:
                    pypiPackageMetadataIndexer.indexAsSystem(event.getPath(), PypiIndexTypeEnum.REINDEX);
                    break;
                case 16:
                    pypiPackageMetadataIndexer.indexAsSystem(event.getPath(), PypiIndexTypeEnum.DELETE);
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
