package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.springframework.stereotype.Component;

@Component
public class ConanCoordinatesAdapter extends LayoutCoordinatesAdapter<ConanCoordinates, ComparableVersion> {

    public ConanCoordinatesAdapter() {
        super(Vertices.CONAN_COORDINATES, ConanCoordinates.class);
    }

    @Override
    protected ConanCoordinates newInstance() {
        return new ConanCoordinates();
    }
}

