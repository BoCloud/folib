package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class NpmCoordinatesAdapter
        extends LayoutCoordinatesAdapter<NpmCoordinates, SemanticVersion>
{

    public NpmCoordinatesAdapter()
    {
        super(Vertices.NPM_COORDINATES, NpmCoordinates.class);
    }

    @Override
    protected NpmCoordinates newInstance()
    {
        return new NpmCoordinates();
    }
    
}
