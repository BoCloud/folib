package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class DebianCoordinatesAdapter extends LayoutCoordinatesAdapter<DebianCoordinates, SemanticVersion> {
    public DebianCoordinatesAdapter() {
        super(Vertices.DEBIAN_COORDINATES, DebianCoordinates.class);
    }

    @Override
    protected DebianCoordinates newInstance() {
        return new DebianCoordinates();
    }


}

