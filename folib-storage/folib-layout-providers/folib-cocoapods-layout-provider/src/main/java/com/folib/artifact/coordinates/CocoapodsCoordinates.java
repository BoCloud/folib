package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;



/**
 * @author veadan
 * @date 2023/7/28 17:44
 */
@NodeEntity(Vertices.COCOAPODS_COORDINATES)
@XmlRootElement(name = "CocoapodsCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = CocoapodsCoordinates.LAYOUT_NAME, alias = CocoapodsCoordinates.LAYOUT_ALIAS)
public class CocoapodsCoordinates extends
        LayoutCoordinatesEntity<CocoapodsCoordinates, String> {

    public static final String LAYOUT_NAME = "cocoapods";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    
    private static final String NAME = "name";
    private static final String BASE_NAME = "base_name";
    private static final String PATH = "path";


    public CocoapodsCoordinates() {
        resetCoordinates(NAME, PATH);
    }
    
    public CocoapodsCoordinates(String name) {
        this();
        
        this.setName(name);
    }
    
    public CocoapodsCoordinates(String name, String path) {
        this();
        
        this.setName(name);
        this.setPath(path);
    }

    @Override
    public String getId() {
        return this.getName();
    }

    @Override
    public String getNativeVersion() {
        return super.getVersion();
    }

    @Override
    public String convertToPath(CocoapodsCoordinates artifactCoordinates) {
        return artifactCoordinates.getId();
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = NAME)
    public String getName()
    {
        return getCoordinate(NAME);
    }

    public void setName(String name)
    {
        setCoordinate(NAME, name);
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = BASE_NAME)
    public String getBaseName()
    {
        return getCoordinate(BASE_NAME);
    }

    public void setBaseName(String baseName) {
        setCoordinate(BASE_NAME, baseName);
    }
    
    @ArtifactLayoutCoordinate
    @XmlAttribute(name = PATH)
    public String getPath()
    {
        return getCoordinate(PATH);
    }

    public void setPath(String path) {
        setCoordinate(PATH, path);
        
    }
}
