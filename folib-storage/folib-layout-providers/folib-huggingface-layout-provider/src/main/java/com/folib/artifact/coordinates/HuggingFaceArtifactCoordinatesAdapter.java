package com.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class HuggingFaceArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<HuggingFaceArtifactCoordinates, HuggingFaceArtifactCoordinates> {

    public HuggingFaceArtifactCoordinatesAdapter() {
        super(Vertices.HUGGINGFACE_ARTIFACT_COORDINATES, HuggingFaceArtifactCoordinates.class);

    }

    @Override
    protected HuggingFaceArtifactCoordinates newInstance() {
        return new HuggingFaceArtifactCoordinates();
    }
}
