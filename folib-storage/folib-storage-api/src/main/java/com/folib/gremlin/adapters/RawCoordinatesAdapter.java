package com.folib.gremlin.adapters;

import com.folib.artifact.coordinates.RawCoordinates;
import com.folib.db.schema.Vertices;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class RawCoordinatesAdapter
        extends LayoutCoordinatesAdapter<RawCoordinates, RawCoordinates>
{

    public RawCoordinatesAdapter()
    {
        super(Vertices.RAW_COORDINATES, RawCoordinates.class);
    }

    @Override
    protected RawCoordinates newInstance()
    {
        return new RawCoordinates();
    }
    
}
