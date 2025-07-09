package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @date 1/3/2024 15:36
 */
@Component
public class GoCoordinatesAdapter
        extends LayoutCoordinatesAdapter<GoCoordinates, SemanticVersion> {

    public GoCoordinatesAdapter() {
        super(Vertices.GO_COORDINATES, GoCoordinates.class);
    }

    @Override
    protected GoCoordinates newInstance() {
        return new GoCoordinates();
    }

}
