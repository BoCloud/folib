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

    public static final String CHANNEL = "channel";
    public static final String PLATFORM = "platform";
    public static final String NAME = "name";
    public static final String VERSION = "version";
    public static final String BUILD = "build";
    public static final String FILE_NAME = "fileName";


    public CondaArtifactCoordinates() {
        resetCoordinates(CHANNEL, NAME, VERSION, PLATFORM, BUILD, FILE_NAME);
    }

    public CondaArtifactCoordinates(String channel,
                                    String name,
                                    String version,
                                    String platform,
                                    String build,
                                    String fileName) {
        this();
        setChannel(channel);
        setName(name);
        setVersion(version);
        setPlatform(platform);
        setBuild(build);
        setFileName(fileName);
    }

    @Override
    public String getId() {
        return getName();
    }

    @ArtifactLayoutCoordinate
    public String getChannel() {
        return getCoordinate(CHANNEL);
    }

    @ArtifactLayoutCoordinate
    public String getPlatform() {
        return getCoordinate(PLATFORM);
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
    public String getFileName() {
        return getCoordinate(FILE_NAME);
    }



    // setters
    private void setChannel(String channel) {
        setCoordinate(CHANNEL, channel);
    }
    private void setPlatform(String platform) {
        setCoordinate(PLATFORM, platform);
    }
    public void setName(String name) {
        setCoordinate(NAME, name);
    }
    private void setBuild(String build) {
        setCoordinate(BUILD, build);
    }
    private void setFileName(String extension) {
        setCoordinate(FILE_NAME, extension);
    }


    @Override
    public SemanticVersion getNativeVersion() {
        return null;
    }

    // 路径解析: conda/{channel}/{name}/{version}/{platform}/{fileName}
    // 传入路径: conda/{channel}/{name}/{version}/{platform}/{fileName}
    // 文件坐标: conda/{channel}/{name}/{version}/{platform}/{fileName}/FILE
    // 元数据坐标: conda/{channel}/{name}/{version}/{platform}/{fileName}/index.json
    public static CondaArtifactCoordinates parse(String path) {
        try {
            Assert.isTrue(path.startsWith("conda/"), "The path must start with 'conda/'");
            String subPath = path.substring(6);
            String[] parts = subPath.split("/");
            Assert.isTrue(parts.length == 6, "Invalid path format: " + path);
            String channel = parts[0];
            String name = parts[1];
            String version = parts[2];
            String platform = parts[3];
            String fileName = parts[4];
            return new CondaArtifactCoordinates(channel, name, version, platform, fileName, fileName);
        } catch (Exception e) {
            log.error("Failed to parse CondaArtifactCoordinates from path: {}", path, e);
        }
        return null;
    }

    @Override
    public String convertToPath(CondaArtifactCoordinates artifactCoordinates) {
        return String.format("conda/%s/%s/%s/%s/%s", artifactCoordinates.getChannel(),
                artifactCoordinates.getName(), artifactCoordinates.getVersion(),
                artifactCoordinates.getPlatform(), artifactCoordinates.getFileName());
    }
}