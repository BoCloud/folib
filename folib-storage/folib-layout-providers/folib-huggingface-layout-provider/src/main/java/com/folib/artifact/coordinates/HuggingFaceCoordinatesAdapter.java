package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class HuggingFaceCoordinatesAdapter extends LayoutCoordinatesAdapter<HuggingFaceCoordinates, HuggingFaceCoordinates> {

    public HuggingFaceCoordinatesAdapter() {
        super(Vertices.HUGGINGFACE_COORDINATES, HuggingFaceCoordinates.class);

    }

    @Override
    protected HuggingFaceCoordinates newInstance() {
        return new HuggingFaceCoordinates();
    }
}
