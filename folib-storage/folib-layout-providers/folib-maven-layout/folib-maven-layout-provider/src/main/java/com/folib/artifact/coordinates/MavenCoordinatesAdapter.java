package com.folib.artifact.coordinates;

import org.apache.maven.artifact.versioning.ComparableVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class MavenCoordinatesAdapter
        extends LayoutCoordinatesAdapter<MavenCoordinates, ComparableVersion>
{

    public MavenCoordinatesAdapter()
    {
        super(Vertices.MAVEN_COORDINATES, MavenCoordinates.class);
    }

    @Override
    protected MavenCoordinates newInstance()
    {
        return new MavenCoordinates();
    }
    
}
