package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class NpmArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<NpmArtifactCoordinates, SemanticVersion>
{

    public NpmArtifactCoordinatesAdapter()
    {
        super(Vertices.NPM_ARTIFACT_COORDINATES, NpmArtifactCoordinates.class);
    }

    @Override
    protected NpmArtifactCoordinates newInstance()
    {
        return new NpmArtifactCoordinates();
    }
    
}
