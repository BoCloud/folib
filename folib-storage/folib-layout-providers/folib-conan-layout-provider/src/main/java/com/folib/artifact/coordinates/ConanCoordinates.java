package com.folib.artifact.coordinates;


import com.folib.constant.GlobalConstants;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.extern.slf4j.Slf4j;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.neo4j.ogm.annotation.NodeEntity;


import java.net.URI;

@NodeEntity(Vertices.CONAN_COORDINATES)
@XmlRootElement(name = "ConanCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = ConanCoordinates.LAYOUT_NAME, alias = ConanCoordinates.LAYOUT_ALIAS)
@Slf4j
public class ConanCoordinates extends LayoutCoordinatesEntity<ConanCoordinates, ComparableVersion> {
    public static final String LAYOUT_NAME = "conan";
    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String VERSION = "version";
    private static final String PATH = "path";
    private static final String NAME = "name";
    public static final String DESCRIPTION = "description";
    private static final String EXTENSION = "extension";

    public ConanCoordinates() {
        resetCoordinates(NAME);
    }

    public ConanCoordinates(String name) {
        setId(name);
    }

    public static ConanCoordinates parse(String relativizePath) {
        log.info("parse conan relativizePath {}", relativizePath);
        return new ConanCoordinates(relativizePath);
    }

    @Override
    public String getId() {
        return getName();
    }

    public void setId(String id) {
        setCoordinate(NAME, id);
    }

    public String getName() {
        return getCoordinate(NAME);
    }

    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    @Override
    public ComparableVersion getNativeVersion() {
        String versionLocal = getVersion();
        if (versionLocal == null) {
            return null;
        }
        return new ComparableVersion(versionLocal);
    }

    @Override
    public String convertToPath(ConanCoordinates c) {
        return c.getId();
    }

    @Override
    public URI convertToResource(ConanCoordinates c) {
        return URI.create(GlobalConstants.DOWNLOAD.concat(GlobalConstants.SEPARATOR).concat(convertToPath(c)));
    }
}