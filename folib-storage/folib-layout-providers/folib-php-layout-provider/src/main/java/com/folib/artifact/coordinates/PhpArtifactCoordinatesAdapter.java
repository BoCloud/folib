package com.folib.artifact.coordinates;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class PhpArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<PhpArtifactCoordinates, SemanticVersion> {

    public PhpArtifactCoordinatesAdapter() {
        super(Vertices.PHP_ARTIFACT_COORDINATES, PhpArtifactCoordinates.class);
    }

    @Override
    protected PhpArtifactCoordinates newInstance() {
        return new PhpArtifactCoordinates();
    }

}
