package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class RpmArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<RpmArtifactCoordinates, SemanticVersion>
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

