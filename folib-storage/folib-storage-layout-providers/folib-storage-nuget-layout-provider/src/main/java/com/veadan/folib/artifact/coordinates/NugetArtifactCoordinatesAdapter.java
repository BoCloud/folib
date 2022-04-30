package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author sbespalov
 */
@Component
public class NugetArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<NugetArtifactCoordinates, SemanticVersion>
{

    public NugetArtifactCoordinatesAdapter()
    {
        super(Vertices.NUGET_ARTIFACT_COORDINATES, NugetArtifactCoordinates.class);
    }

    @Override
    protected NugetArtifactCoordinates newInstance()
    {
        return new NugetArtifactCoordinates();
    }
    
}
