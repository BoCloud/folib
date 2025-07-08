package com.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class RpmArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<RpmArtifactCoordinates, RpmArtifactCoordinates>
{
    public RpmArtifactCoordinatesAdapter()
    {
        super(Vertices.RPM_ARTIFACT_COORDINATES, RpmArtifactCoordinates.class);
    }

    @Override
    protected RpmArtifactCoordinates newInstance()
    {
        return new RpmArtifactCoordinates();
    }

}

