package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinatesLayout;
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
        extends LayoutArtifactCoordinatesEntity<CondaArtifactCoordinates, String> {

    public static final String LAYOUT_NAME = "Conda";

    public static final String LAYOUT_ALIAS = "Conda";

    public static final String NAME = "name";

    public static final String VERSION = "version";

    public static final String BUILD = "build";

    public static final String CHECKSUM_SHA256 = "checksumSha256";

    public static final String ARTIFACT_PATH = "path";

    public CondaArtifactCoordinates() {
        super();
    }

    @Override
    public String getId() {
        return "";
    }

    @Override
    public String getNativeVersion() {
        return "";
    }

    @Override
    public String convertToPath(CondaArtifactCoordinates artifactCoordinates) {
        return "";
    }
}