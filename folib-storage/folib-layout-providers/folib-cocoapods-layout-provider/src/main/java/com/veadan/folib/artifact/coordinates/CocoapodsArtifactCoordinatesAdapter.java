package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @date 2023/8/1 16:00
 */
@Component
public class CocoapodsArtifactCoordinatesAdapter extends
        LayoutArtifactCoordinatesAdapter<CocoapodsArtifactCoordinates, String> {

    public CocoapodsArtifactCoordinatesAdapter() {
        super(Vertices.COCOAPODS_ARTIFACT_COORDINATES, CocoapodsArtifactCoordinates.class);

    }

    @Override
    protected CocoapodsArtifactCoordinates newInstance() {
        return new CocoapodsArtifactCoordinates();
    }
}
