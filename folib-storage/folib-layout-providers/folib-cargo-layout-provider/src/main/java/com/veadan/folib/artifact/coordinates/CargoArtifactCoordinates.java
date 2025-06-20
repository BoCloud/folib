package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;



@NodeEntity(Vertices.CARGO_ARTIFACT_COORDINATES)
@XmlRootElement(name = "CargoArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = CargoArtifactCoordinates.LAYOUT_NAME, alias = CargoArtifactCoordinates.LAYOUT_ALIAS)
public class CargoArtifactCoordinates extends LayoutArtifactCoordinatesEntity<CargoArtifactCoordinates, CargoArtifactCoordinates> {


    public static final String LAYOUT_NAME = "cargo";
    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";
    private static final String SCOPE = "scope";
    private static final String NAME = "name";
    private static final String VERSION = "version";
    private static final String FILENAME = "filename";
    private static final String EXTENSION = "extension";

    public CargoArtifactCoordinates() {
        resetCoordinates(PATH, SCOPE, NAME, VERSION, FILENAME, EXTENSION);
    }

    public CargoArtifactCoordinates(String path) {
        setCoordinate(PATH, path);
    }

    @Override
    public String getId() {
        return getCoordinate(PATH);
    }

    public void setId(String id) {
        setCoordinate(PATH, id);
    }

    @Override
    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "path")
    public String getPath() {
        return getId();
    }

    /**
     * WARNING: Unsurprisingly, this is null.
     *
     * @return null
     */
    @Override
    public String getVersion() {
        return null;
    }

    @Override
    public void setVersion(String version) {
    }

    @Override
    public CargoArtifactCoordinates getNativeVersion() {
        return this;
    }

    @Override
    public String convertToPath(CargoArtifactCoordinates artifactCoordinates) {
        return artifactCoordinates.getId();
    }
}
