package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author LingengMa
 * @date 2025/04/02 16:52
 * @Description:
 */
@Component
public class CondaArtifactCoordinatesAdapter
        extends LayoutArtifactCoordinatesAdapter<CondaArtifactCoordinates, SemanticVersion> {

    public CondaArtifactCoordinatesAdapter() {
        super(Vertices.CONDA_ARTIFACT_COORDINATES, CondaArtifactCoordinates.class);
    }

    @Override
    protected CondaArtifactCoordinates newInstance() {
        return new CondaArtifactCoordinates();
    }
}
