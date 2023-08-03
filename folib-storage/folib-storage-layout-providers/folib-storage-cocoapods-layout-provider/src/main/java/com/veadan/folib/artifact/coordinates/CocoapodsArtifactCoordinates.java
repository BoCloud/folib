package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/7/28 17:44
 * @since x.x.x
 */
@NodeEntity(Vertices.RPM_ARTIFACT_COORDINATES)
@XmlRootElement(name = "CocoapodsArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = CocoapodsArtifactCoordinates.LAYOUT_NAME, alias = CocoapodsArtifactCoordinates.LAYOUT_ALIAS)
public class CocoapodsArtifactCoordinates extends 
        LayoutArtifactCoordinatesEntity<CocoapodsArtifactCoordinates, String> {

    public static final String LAYOUT_NAME = "CocoaPods";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    
    private static final String POD_NAME = "podName";


    public CocoapodsArtifactCoordinates() {
        resetCoordinates(POD_NAME);
    }
    
    public CocoapodsArtifactCoordinates(String podName) {
        this();
        
        this.setPodName(podName);
    }
    

    @Override
    public String getId() {
        return this.getPodName();
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
    @XmlAttribute(name = POD_NAME)
    public String getPodName()
    {
        return getCoordinate(POD_NAME);
    }

    public void setPodName(String podName)
    {
        setCoordinate(POD_NAME, podName);
    }
}
