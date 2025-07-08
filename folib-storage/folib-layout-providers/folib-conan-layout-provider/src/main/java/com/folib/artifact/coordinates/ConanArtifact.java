package com.folib.artifact.coordinates;

import com.folib.domain.Artifact;
import com.folib.providers.io.RepositoryPath;

public interface ConanArtifact extends Artifact {

    RepositoryPath getPath();

    void setPath(RepositoryPath destination);
}