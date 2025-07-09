package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class PypiCoordinatesAdapter
        extends LayoutCoordinatesAdapter<PypiCoordinates, SemanticVersion>
{

    public PypiCoordinatesAdapter()
    {
        super(Vertices.PYPI_COORDINATES, PypiCoordinates.class);
    }

    @Override
    protected PypiCoordinates newInstance()
    {
        return new PypiCoordinates();
    }
    
}
