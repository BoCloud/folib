package com.folib.gremlin.adapters;


import com.folib.artifact.coordinates.GitLfsCoordinates;
import com.folib.db.schema.Vertices;
import org.springframework.stereotype.Component;

@Component
public class GitLfsCoordinatesAdapter extends LayoutCoordinatesAdapter<GitLfsCoordinates, GitLfsCoordinates> {

    public GitLfsCoordinatesAdapter() {
        super(Vertices.GITLFS_COORDINATES, GitLfsCoordinates.class);
    }

    @Override
    protected GitLfsCoordinates newInstance() {
        return new GitLfsCoordinates();
    }


}
