package com.veadan.folib.artifact.coordinates;

import org.apache.maven.artifact.versioning.ComparableVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class MavenArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<MavenArtifactCoordinates, ComparableVersion>
{

    public MavenArtifactCoordinatesAdapter()
    {
        super(Vertices.MAVEN_ARTIFACT_COORDINATES, MavenArtifactCoordinates.class);
    }

    @Override
    protected MavenArtifactCoordinates newInstance()
    {
        return new MavenArtifactCoordinates();
    }
    
}
