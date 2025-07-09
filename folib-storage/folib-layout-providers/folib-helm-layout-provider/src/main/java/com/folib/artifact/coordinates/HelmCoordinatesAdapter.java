package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.springframework.stereotype.Component;

@Component
public class HelmCoordinatesAdapter extends LayoutCoordinatesAdapter<HelmCoordinates, ComparableVersion> {

    public HelmCoordinatesAdapter()
    {
        super(Vertices.HELM_COORDINATES, HelmCoordinates.class);// todo
    }

    @Override
    protected HelmCoordinates newInstance()
    {
        return new HelmCoordinates();
    }
}
