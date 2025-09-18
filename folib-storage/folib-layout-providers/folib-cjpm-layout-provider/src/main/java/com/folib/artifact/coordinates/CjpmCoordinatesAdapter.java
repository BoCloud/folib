package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

@Component
public class CjpmCoordinatesAdapter
        extends LayoutCoordinatesAdapter<CjpmCoordinates, CjpmCoordinates>
{

    public CjpmCoordinatesAdapter()
    {
        super(Vertices.CJPM_COORDINATES, CjpmCoordinates.class);
    }

    @Override
    protected CjpmCoordinates newInstance()
    {
        return new CjpmCoordinates();
    }

}
