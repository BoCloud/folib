package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class PypiArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<PypiArtifactCoordinates, SemanticVersion>
{

    public PypiArtifactCoordinatesAdapter()
    {
        super(Vertices.PYPI_ARTIFACT_COORDINATES, PypiArtifactCoordinates.class);
    }

    @Override
    protected PypiArtifactCoordinates newInstance()
    {
        return new PypiArtifactCoordinates();
    }
    
}
