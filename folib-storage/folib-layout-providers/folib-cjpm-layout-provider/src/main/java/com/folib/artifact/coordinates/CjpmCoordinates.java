package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;

@NodeEntity(Vertices.CJPM_COORDINATES)
@XmlRootElement(name = "CjpmCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = CjpmCoordinates.LAYOUT_NAME, alias = CjpmCoordinates.LAYOUT_ALIAS)
public class CjpmCoordinates
        extends LayoutCoordinatesEntity<CjpmCoordinates, CjpmCoordinates>
{

    public static final String LAYOUT_NAME = "cjpm";
    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    public CjpmCoordinates()
    {
        resetCoordinates(PATH);
    }

    public CjpmCoordinates(String path)
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

    @Override
    public CjpmCoordinates getNativeVersion()
    {
        return this;
    }

    @Override
    public String convertToPath(CjpmCoordinates artifactCoordinates)
    {
        return artifactCoordinates.getId();
    }

}