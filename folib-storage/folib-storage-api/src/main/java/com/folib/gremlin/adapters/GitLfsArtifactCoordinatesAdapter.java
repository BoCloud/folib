package com.folib.gremlin.adapters;


import com.folib.artifact.coordinates.GitLfsArtifactCoordinates;
import com.veadan.folib.db.schema.Vertices;
import org.springframework.stereotype.Component;

@Component
public class GitLfsArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<GitLfsArtifactCoordinates, GitLfsArtifactCoordinates> {

    public GitLfsArtifactCoordinatesAdapter() {
        super(Vertices.GITLFS_ARTIFACT_COORDINATES, GitLfsArtifactCoordinates.class);
    }

    @Override
    protected GitLfsArtifactCoordinates newInstance() {
        return new GitLfsArtifactCoordinates();
    }


}
