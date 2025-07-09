package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class NugetCoordinatesAdapter
        extends LayoutCoordinatesAdapter<NugetCoordinates, SemanticVersion>
{

    public NugetCoordinatesAdapter()
    {
        super(Vertices.NUGET_COORDINATES, NugetCoordinates.class);
    }

    @Override
    protected NugetCoordinates newInstance()
    {
        return new NugetCoordinates();
    }
    
}
