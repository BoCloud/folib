package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.springframework.stereotype.Component;

@Component
public class ConanArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<ConanArtifactCoordinates, ComparableVersion> {

    public ConanArtifactCoordinatesAdapter() {
        super(Vertices.CONAN_ARTIFACT_COORDINATES, ConanArtifactCoordinates.class);
    }

    @Override
    protected ConanArtifactCoordinates newInstance() {
        return new ConanArtifactCoordinates();
    }
}

