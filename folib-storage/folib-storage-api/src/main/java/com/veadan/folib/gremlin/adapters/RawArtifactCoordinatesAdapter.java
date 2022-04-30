package com.veadan.folib.gremlin.adapters;

import com.veadan.folib.artifact.coordinates.RawArtifactCoordinates;
import com.veadan.folib.db.schema.Vertices;
import org.springframework.stereotype.Component;

/**
 * @author sbespalov
 */
@Component
public class RawArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<RawArtifactCoordinates, RawArtifactCoordinates>
{

    public RawArtifactCoordinatesAdapter()
    {
        super(Vertices.RAW_ARTIFACT_COORDINATES, RawArtifactCoordinates.class);
    }

    @Override
    protected RawArtifactCoordinates newInstance()
    {
        return new RawArtifactCoordinates();
    }
    
}
