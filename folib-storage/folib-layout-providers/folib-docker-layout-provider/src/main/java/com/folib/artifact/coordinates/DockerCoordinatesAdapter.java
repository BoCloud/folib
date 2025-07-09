package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class DockerCoordinatesAdapter extends LayoutCoordinatesAdapter<DockerCoordinates, String>
{
    public DockerCoordinatesAdapter()
    {
        super(Vertices.DOCKER_COORDINATES, DockerCoordinates.class);
    }

    @Override
    protected DockerCoordinates newInstance()
    {
        return new DockerCoordinates();
    }

}
