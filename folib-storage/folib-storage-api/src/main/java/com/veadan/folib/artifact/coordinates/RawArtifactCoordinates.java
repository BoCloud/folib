package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author Veadan
 */
@NodeEntity(Vertices.RAW_ARTIFACT_COORDINATES)
@ArtifactCoordinatesLayout(name = RawArtifactCoordinates.LAYOUT_NAME, alias = RawArtifactCoordinates.LAYOUT_ALIAS)
public class RawArtifactCoordinates
        extends LayoutArtifactCoordinatesEntity<RawArtifactCoordinates, RawArtifactCoordinates>
{

    public static final String LAYOUT_NAME = "Raw";
    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    public RawArtifactCoordinates()
    {
        resetCoordinates(PATH);
    }

    public RawArtifactCoordinates(String path)
    {
        setCoordinate(PATH, path);
    }

    @Override
    public String getId()
    {
        return getCoordinate(PATH);
    }

    public void setId(String id)
    {
        setCoordinate(PATH, id);
    }

    @ArtifactLayoutCoordinate
    public String getPath() 
    {
        return getId();
    }
    
    /**
     * WARNING: Unsurprisingly, this is null.
     * @return  null
     */
    @Override
    public String getVersion()
    {
        return null;
    }

    @Override
    public void setVersion(String version)
    {
    }

    @Override
    public RawArtifactCoordinates getNativeVersion()
    {
        return this;
    }

    @Override
    public String convertToPath(RawArtifactCoordinates artifactCoordinates)
    {
        return artifactCoordinates.getId();
    }

}
