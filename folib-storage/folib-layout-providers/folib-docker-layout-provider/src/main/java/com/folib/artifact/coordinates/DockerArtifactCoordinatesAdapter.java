package com.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class DockerArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<DockerArtifactCoordinates, String>
{
    public DockerArtifactCoordinatesAdapter()
    {
        super(Vertices.DOCKER_ARTIFACT_COORDINATES, DockerArtifactCoordinates.class);
    }

    @Override
    protected DockerArtifactCoordinates newInstance()
    {
        return new DockerArtifactCoordinates();
    }

}
