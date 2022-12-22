package com.veadan.folib.artifact.coordinates;


import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import lombok.extern.slf4j.Slf4j;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;

@NodeEntity(Vertices.CONAN_ARTIFACT_COORDINATES)
@XmlRootElement(name = "conanArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = "conan", alias = "conan")
@Slf4j
public class ConanArtifactCoordinates extends LayoutArtifactCoordinatesEntity<ConanArtifactCoordinates, ComparableVersion> {
    private static final String VERSION = "version";
    private static final String PATH = "path";
    private static final String NAME = "name";
    public static final String DESCRIPTION = "description";
    private static final String EXTENSION = "extension";


    public ConanArtifactCoordinates() {
        resetCoordinates(NAME);
    }

    public ConanArtifactCoordinates(String name) {
        setId(name);
    }

    public static ConanArtifactCoordinates parse(String relativizePath) {
        log.debug("parse conan relativizePath {}", relativizePath);
        return new ConanArtifactCoordinates(relativizePath);
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
    public String convertToPath(ConanArtifactCoordinates c) {
        return c.getId();
    }

    @Override
    public URI convertToResource(ConanArtifactCoordinates c) {
        return URI.create(convertToPath(c));
    }
}