package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author xuxinping
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
