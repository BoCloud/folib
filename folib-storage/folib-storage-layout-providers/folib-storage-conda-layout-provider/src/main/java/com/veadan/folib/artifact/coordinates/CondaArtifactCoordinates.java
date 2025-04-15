package com.veadan.folib.artifact.coordinates;

import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinatesLayout;
import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.List;

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

    public static final String PLATFORM = "platform";
    public static final String FILE_NAME = "fileName";


    public CondaArtifactCoordinates() {
        resetCoordinates(PLATFORM, FILE_NAME);
    }

    public CondaArtifactCoordinates(String platform,
                                    String fileName) {
        this();
        setPlatform(platform);
        setFileName(fileName);
    }

    @Override
    public String getId() {
        return getFileName();
    }

    // getters
    @ArtifactLayoutCoordinate
    public String getPlatform() {
        return getCoordinate(PLATFORM);
    }

    @ArtifactLayoutCoordinate
    public String getFileName() {
        return getCoordinate(FILE_NAME);
    }



    // setters
    private void setPlatform(String platform) {
        setCoordinate(PLATFORM, platform);
    }
    private void setFileName(String extension) {
        setCoordinate(FILE_NAME, extension);
    }


    @Override
    public SemanticVersion getNativeVersion() {
        return null;
    }

    // 路径解析:
    // 文件路径: {platform}/{fileName}
    public static CondaArtifactCoordinates parse(String path) {
        Assert.notNull(path, "path cannot be null");
        String[] parts = path.split("/");
        if (parts.length != 2) {
            throw new IllegalArgumentException("Invalid path format: " + path);
        }
        String platform = parts[0];
        String fileName = parts[1];
        return new CondaArtifactCoordinates(platform, fileName);
    }

    @Override
    public String convertToPath(CondaArtifactCoordinates artifactCoordinates) {
        String platform = artifactCoordinates.getPlatform();
        String fileName = artifactCoordinates.getFileName();
        return String.format("%s/%s", platform, fileName);
    }
}