package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class DebianArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<DebianArtifactCoordinates, SemanticVersion> {
    public DebianArtifactCoordinatesAdapter() {
        super(Vertices.DEBIAN_ARTIFACT_COORDINATES, DebianArtifactCoordinates.class);
    }

    @Override
    protected DebianArtifactCoordinates newInstance() {
        return new DebianArtifactCoordinates();
    }


}

