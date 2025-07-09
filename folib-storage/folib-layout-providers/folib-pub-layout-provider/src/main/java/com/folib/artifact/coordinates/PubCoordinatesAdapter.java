package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class PubCoordinatesAdapter
        extends LayoutCoordinatesAdapter<PubCoordinates, SemanticVersion> {

    public PubCoordinatesAdapter() {
        super(Vertices.PUB_COORDINATES, PubCoordinates.class);
    }

    @Override
    protected PubCoordinates newInstance() {
        return new PubCoordinates();
    }

}
