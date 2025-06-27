package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
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
