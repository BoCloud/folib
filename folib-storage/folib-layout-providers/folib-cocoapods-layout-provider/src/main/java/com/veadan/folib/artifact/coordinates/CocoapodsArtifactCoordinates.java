package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;



/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/7/28 17:44
 * @since x.x.x
 */
@NodeEntity(Vertices.COCOAPODS_ARTIFACT_COORDINATES)
@XmlRootElement(name = "CocoapodsArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = CocoapodsArtifactCoordinates.LAYOUT_NAME, alias = CocoapodsArtifactCoordinates.LAYOUT_ALIAS)
public class CocoapodsArtifactCoordinates extends 
        LayoutArtifactCoordinatesEntity<CocoapodsArtifactCoordinates, String> {

    public static final String LAYOUT_NAME = "cocoapods";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    
    private static final String NAME = "name";
    private static final String BASE_NAME = "base_name";
    private static final String PATH = "path";


    public CocoapodsArtifactCoordinates() {
        resetCoordinates(NAME, PATH);
    }
    
    public CocoapodsArtifactCoordinates(String name) {
        this();
        
        this.setName(name);
    }
    
    public CocoapodsArtifactCoordinates(String name, String path) {
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
    public String convertToPath(CocoapodsArtifactCoordinates artifactCoordinates) {
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
