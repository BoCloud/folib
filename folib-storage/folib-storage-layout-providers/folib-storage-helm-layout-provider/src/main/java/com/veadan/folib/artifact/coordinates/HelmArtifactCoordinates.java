package com.veadan.folib.artifact.coordinates;


import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;

@NodeEntity(Vertices.HELM_ARTIFACT_COORDINATES)
@XmlRootElement(name = "HelmArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = "helm", alias = "helm")
@Slf4j
@Data
public class HelmArtifactCoordinates extends LayoutArtifactCoordinatesEntity<HelmArtifactCoordinates, ComparableVersion> {
    private String LAYOUT_NAME = "helm";
    private String LAYOUT_ALIAS = "helm";
    private String BASE_NAME = "base_name";
    private String PACKAGE_TYPE = "Chart";
    private String META_DATA = "false";// index.yaml
    private String ARTIFACT_SUFFIX = ".tgz";// .tgz.prov
    private static final String VERSION = "version";
    private static final String PATH = "path";
    private static final String NAME = "name";
    public static final String DESCRIPTION = "description";
    private static final String EXTENSION = "extension";

    public HelmArtifactCoordinates() {
        resetCoordinates(NAME);
    }

    public HelmArtifactCoordinates(String name) {
        setId(name);
    }

    public static HelmArtifactCoordinates parse(String relativizePath) {
        log.info("parse helm relativizePath {}", relativizePath);
        return new HelmArtifactCoordinates(relativizePath);
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
    public String convertToPath(HelmArtifactCoordinates c) {
        return c.getId();
    }

    @Override
    public URI convertToResource(HelmArtifactCoordinates c) {
        return URI.create(convertToPath(c));
    }

}
