package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.providers.io.RepositoryPath;
import org.apache.maven.artifact.Artifact;

public interface HelmArtifact extends Artifact {

    RepositoryPath getPath();

    void setPath(RepositoryPath destination);
}
