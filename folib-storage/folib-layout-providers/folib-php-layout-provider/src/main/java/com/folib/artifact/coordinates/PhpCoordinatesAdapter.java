package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class PhpCoordinatesAdapter
        extends LayoutCoordinatesAdapter<PhpCoordinates, SemanticVersion> {

    public PhpCoordinatesAdapter() {
        super(Vertices.PHP_COORDINATES, PhpCoordinates.class);
    }

    @Override
    protected PhpCoordinates newInstance() {
        return new PhpCoordinates();
    }

}
