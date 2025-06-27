package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
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
