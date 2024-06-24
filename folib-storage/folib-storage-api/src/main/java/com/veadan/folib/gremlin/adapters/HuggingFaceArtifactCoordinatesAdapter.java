package com.veadan.folib.gremlin.adapters;

import com.veadan.folib.artifact.coordinates.HuggingFaceArtifactCoordinates;
import com.veadan.folib.db.schema.Vertices;
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
