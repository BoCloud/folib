package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;

public interface ConanArtifact extends Artifact {

    RepositoryPath getPath();

    void setPath(RepositoryPath destination);
}