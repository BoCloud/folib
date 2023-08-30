package com.veadan.folib.providers.repository;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.AsyncArtifactEntryHandler;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.util.LocalDateTimeInstance;
import jnr.ffi.annotations.In;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

@Slf4j
@Component
public class ArtifactDownloadingEventHandler extends AsyncArtifactEntryHandler {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

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
        String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + "-metadata";
        RepositoryPath artifactRepositoryPath = repositoryPath.getParent().resolve(fileName);
        Files.writeString(artifactRepositoryPath, JSONObject.toJSONString(artifactEntry));
        Artifact updateArtifactEntry = new ArtifactEntity(artifactEntry.getNativeId(), artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getUuid(), artifactEntry.getArtifactCoordinates());
        updateArtifactEntry.setDownloadCount(artifactEntry.getDownloadCount() + 1);
        updateArtifactEntry.setLastUsed(LocalDateTimeInstance.now());
        log.warn("[{}] [{}] downloadCount changed from [{}] to [{}].",
                this.getClass().getSimpleName(),
                repositoryPath,
                artifactEntry.getDownloadCount(),
                updateArtifactEntry.getDownloadCount()
        );
        return updateArtifactEntry;
    }

}
