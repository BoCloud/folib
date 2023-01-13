package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.springframework.stereotype.Component;

@Component
public class HelmArtifactCoordinatesAdapter extends LayoutArtifactCoordinatesAdapter<HelmArtifactCoordinates, ComparableVersion> {

    public HelmArtifactCoordinatesAdapter()
    {
        super(Vertices.HELM_ARTIFACT_COORDINATES, HelmArtifactCoordinates.class);// todo
    }

    @Override
    protected HelmArtifactCoordinates newInstance()
    {
        return new HelmArtifactCoordinates();
    }
}
