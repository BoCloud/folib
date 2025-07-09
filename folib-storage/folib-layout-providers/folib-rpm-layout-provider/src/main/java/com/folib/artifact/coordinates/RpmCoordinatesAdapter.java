package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class RpmCoordinatesAdapter extends LayoutCoordinatesAdapter<RpmCoordinates, RpmCoordinates>
{
    public RpmCoordinatesAdapter()
    {
        super(Vertices.RPM_COORDINATES, RpmCoordinates.class);
    }

    @Override
    protected RpmCoordinates newInstance()
    {
        return new RpmCoordinates();
    }

}

