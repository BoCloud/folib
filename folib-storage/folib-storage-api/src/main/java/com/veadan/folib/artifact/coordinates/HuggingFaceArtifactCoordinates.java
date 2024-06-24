package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;

@NodeEntity(Vertices.HUGGINGFACE_ARTIFACT_COORDINATES)
@XmlRootElement(name = "HuggingFaceArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = HuggingFaceArtifactCoordinates.LAYOUT_NAME, alias = HuggingFaceArtifactCoordinates.LAYOUT_ALIAS)
public class HuggingFaceArtifactCoordinates extends LayoutArtifactCoordinatesEntity<HuggingFaceArtifactCoordinates, HuggingFaceArtifactCoordinates> {

    public static final String LAYOUT_NAME = "HuggingFace";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    public HuggingFaceArtifactCoordinates() {
        resetCoordinates(PATH);
    }
    public HuggingFaceArtifactCoordinates(String path) {
        setCoordinate(PATH, path);
    }
    public String getId() {
        return getCoordinate(PATH);
    }

    public void setId(String id) {
        setCoordinate(PATH, id);
    }
    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "path")
    public String getPath() {
        return getCoordinate(PATH);
    }
    @Override
    public HuggingFaceArtifactCoordinates getNativeVersion() {
        return null;
    }
    @Override
    public String convertToPath(HuggingFaceArtifactCoordinates artifactCoordinates) {
        return artifactCoordinates.getId();
    }
}
