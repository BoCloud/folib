package com.folib.artifact.coordinates;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;


/**
 * @author Veadan
 */
@NodeEntity(Vertices.RAW_COORDINATES)
@XmlRootElement(name = "RawArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = RawCoordinates.LAYOUT_NAME, alias = RawCoordinates.LAYOUT_ALIAS)
public class RawCoordinates
        extends LayoutCoordinatesEntity<RawCoordinates, RawCoordinates>
{

    public static final String LAYOUT_NAME = "Raw";
    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    public RawCoordinates()
    {
        resetCoordinates(PATH);
    }

    public RawCoordinates(String path)
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
    public RawCoordinates getNativeVersion()
    {
        return this;
    }

    @Override
    public String convertToPath(RawCoordinates artifactCoordinates)
    {
        return artifactCoordinates.getId();
    }

}
