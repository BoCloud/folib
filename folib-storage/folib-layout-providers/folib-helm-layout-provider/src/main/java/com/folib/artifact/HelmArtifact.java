package com.folib.artifact;

import com.folib.providers.io.RepositoryPath;
import org.apache.maven.artifact.Artifact;

public interface HelmArtifact extends Artifact {

    RepositoryPath getPath();

    void setPath(RepositoryPath destination);
}
