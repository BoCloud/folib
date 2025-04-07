package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinatesLayout;
import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import lombok.extern.slf4j.Slf4j;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author LingengMa
 */
@Slf4j
@NodeEntity(Vertices.CONDA_ARTIFACT_COORDINATES)
@XmlRootElement(name = "CondaArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = CondaArtifactCoordinates.LAYOUT_NAME, alias = CondaArtifactCoordinates.LAYOUT_ALIAS)
public class CondaArtifactCoordinates
        extends LayoutArtifactCoordinatesEntity<CondaArtifactCoordinates, SemanticVersion> {

    public static final String LAYOUT_NAME = "conda";
    public static final String LAYOUT_ALIAS = "conda";

    public static final String NAME = "name";
    public static final String VERSION = "version";
    public static final String BUILD = "build";
    public static final String BUILD_NUMBER = "build_number";
    public static final String PLATFORM = "platform";

    public CondaArtifactCoordinates() {
        resetCoordinates(NAME, VERSION, BUILD, BUILD_NUMBER, PLATFORM);
    }

    public CondaArtifactCoordinates(String name, String version, String build, String buildNumber, String platform) {
        this();
        setName(name);
        setVersion(version);
        setBuild(build);
        setBuildNumber(buildNumber);
        setPlatform(platform);
    }

    // 路径解析
    public static CondaArtifactCoordinates parse(String path) {
        return null;
    }

    @Override
    public String getId() {
        return getName();
    }

    // getters
    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }

    @ArtifactLayoutCoordinate
    public String getVersion() {
        return getCoordinate(VERSION);
    }

    @ArtifactLayoutCoordinate
    public String getBuild() {
        return getCoordinate(BUILD);
    }

    @ArtifactLayoutCoordinate
    public String getBuildNumber() {
        return getCoordinate(BUILD_NUMBER);
    }

    @ArtifactLayoutCoordinate
    public String getPlatform() {
        return getCoordinate(PLATFORM);
    }


    // setters
    public void setName(String name) {
        setCoordinate(NAME, name);
    }
    private void setBuild(String build) {
        setCoordinate(BUILD, build);
    }
    private void setBuildNumber(String buildNumber) {
        setCoordinate(BUILD_NUMBER, buildNumber);
    }
    private void setPlatform(String platform) {
        setCoordinate(PLATFORM, platform);
    }


    @Override
    public SemanticVersion getNativeVersion() {
        return null;
    }

    @Override
    public String convertToPath(CondaArtifactCoordinates artifactCoordinates) {
        return "";
    }
}