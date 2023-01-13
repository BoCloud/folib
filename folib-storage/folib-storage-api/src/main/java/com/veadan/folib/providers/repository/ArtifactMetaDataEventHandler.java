package com.veadan.folib.providers.repository;

import com.veadan.folib.artifact.AsyncArtifactEntryHandler;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;

@Component
@Slf4j
public class ArtifactMetaDataEventHandler extends AsyncArtifactEntryHandler {

    public ArtifactMetaDataEventHandler() {
        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE);
    }

    @Override
    protected Artifact handleEvent(RepositoryPath repositoryPath) throws IOException {
        long size = Files.size(repositoryPath);

        Artifact artifactEntry = repositoryPath.getArtifactEntry();
        log.info("<<<< 当前元数据 {}", artifactEntry.getMetadata());
        artifactEntry.setLastUpdated(LocalDateTimeInstance.now());
        artifactEntry.setSizeInBytes(size);

        return artifactEntry;
    }

}
