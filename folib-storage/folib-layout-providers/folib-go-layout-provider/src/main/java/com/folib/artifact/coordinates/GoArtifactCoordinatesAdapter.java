package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @date 1/3/2024 15:36
 */
@Component
public class GoArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<GoArtifactCoordinates, SemanticVersion> {

    public GoArtifactCoordinatesAdapter() {
        super(Vertices.GO_ARTIFACT_COORDINATES, GoArtifactCoordinates.class);
    }

    @Override
    protected GoArtifactCoordinates newInstance() {
        return new GoArtifactCoordinates();
    }

}
