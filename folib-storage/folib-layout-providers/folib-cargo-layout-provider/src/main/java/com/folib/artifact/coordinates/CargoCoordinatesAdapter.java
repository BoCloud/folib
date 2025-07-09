package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class CargoCoordinatesAdapter extends LayoutCoordinatesAdapter<CargoCoordinates, CargoCoordinates> {

    public CargoCoordinatesAdapter() {
        super(Vertices.CARGO_COORDINATES, CargoCoordinates.class);
    }

    @Override
    protected CargoCoordinates newInstance() {
        return new CargoCoordinates();
    }
}
