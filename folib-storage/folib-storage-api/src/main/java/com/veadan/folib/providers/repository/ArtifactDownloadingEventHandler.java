package com.veadan.folib.providers.repository;

import com.veadan.folib.artifact.AsyncArtifactEntryHandler;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.util.LocalDateTimeInstance;

import java.io.IOException;

import org.springframework.stereotype.Component;

@Component
public class ArtifactDownloadingEventHandler extends AsyncArtifactEntryHandler
{

    public ArtifactDownloadingEventHandler()
    {
        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADING);
    }

    @Override
    protected Artifact handleEvent(RepositoryPath repositoryPath) throws IOException
    {
        Artifact artifactEntry = repositoryPath.getArtifactEntry();
        
        artifactEntry.setDownloadCount(artifactEntry.getDownloadCount() + 1);
        artifactEntry.setLastUsed(LocalDateTimeInstance.now());

        return artifactEntry;
    }

}
