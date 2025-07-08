package com.folib.event;

import com.folib.constant.DebianConstant;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.indexer.DebianIncrementalIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author veadan
 */
@Component
public class DebianArtifactEventListener {

    @Inject
    private DebianIncrementalIndexer debianIncrementalIndexer;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!DebianLayoutProvider.ALIAS.equals(repository.getLayout()) || !RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }
        //  删除事件更新索引
        if (event.getType() == ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() || event.getType() == ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType()) {
            if (event.getPath().getPath().startsWith(DebianConstant.DEB_PREFIX)) {
                debianIncrementalIndexer.removeByPath(repository, event.getPath().getPath());
            }
        }
    }

    Repository getRepository(final ArtifactEvent<RepositoryPath> event) {
        return event.getPath().getFileSystem().getRepository();
    }
}
