package com.veadan.folib.providers.repository;

import com.veadan.folib.artifact.AsyncArtifactEntryHandler;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Slf4j
@Component
public class ArtifactDownloadingEventHandler extends AsyncArtifactEntryHandler {

    public ArtifactDownloadingEventHandler() {
        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADING);
    }

    @Override
    protected Artifact handleEvent(RepositoryPath repositoryPath) throws IOException {
        Artifact artifactEntry = repositoryPath.getArtifactEntry();
        if (artifactEntry == null) {
            log.warn("No [{}] for [{}].",
                    Artifact.class.getSimpleName(),
                    repositoryPath);

            return null;
        }
        Artifact updateArtifactEntry = new ArtifactEntity(artifactEntry.getNativeId(), artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getUuid(), artifactEntry.getArtifactCoordinates());
        updateArtifactEntry.setDownloadCount(artifactEntry.getDownloadCount() + 1);
        updateArtifactEntry.setLastUsed(LocalDateTimeInstance.now());

        return updateArtifactEntry;
    }

}
