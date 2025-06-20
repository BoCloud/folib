package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class CargoArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<CargoArtifactCoordinates, CargoArtifactCoordinates> {

    public CargoArtifactCoordinatesAdapter() {
        super(Vertices.CARGO_ARTIFACT_COORDINATES, CargoArtifactCoordinates.class);
    }

    @Override
    protected CargoArtifactCoordinates newInstance() {
        return new CargoArtifactCoordinates();
    }
}
