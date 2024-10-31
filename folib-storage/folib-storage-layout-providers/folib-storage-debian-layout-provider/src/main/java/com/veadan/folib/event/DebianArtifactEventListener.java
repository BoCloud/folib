package com.veadan.folib.event;

import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.indexer.DebianIncrementalIndexer;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DebianLayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
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
