package com.folib.artifact.coordinates;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.veadan.folib.db.schema.Vertices;
import com.folib.domain.LayoutArtifactCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;


/**
 * @author Veadan
 */
@NodeEntity(Vertices.RAW_ARTIFACT_COORDINATES)
@XmlRootElement(name = "RawArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
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

    @Override
    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "path")
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

    @JsonIgnore
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
