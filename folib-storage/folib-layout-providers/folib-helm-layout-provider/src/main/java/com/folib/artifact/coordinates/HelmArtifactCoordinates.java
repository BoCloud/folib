package com.folib.artifact.coordinates;


import com.veadan.folib.db.schema.Vertices;
import com.folib.domain.LayoutArtifactCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.neo4j.ogm.annotation.NodeEntity;


import java.net.URI;

@NodeEntity(Vertices.HELM_ARTIFACT_COORDINATES)
@XmlRootElement(name = "HelmArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = "helm", alias = "helm")
@Slf4j
@Data
public class HelmArtifactCoordinates extends LayoutArtifactCoordinatesEntity<HelmArtifactCoordinates, ComparableVersion> {
    public static String LAYOUT_NAME = "helm";
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

    public HelmArtifactCoordinates(String relativizePath,String packageName) {

        // 正则表达式匹配 Helm 包名
//        String regex = "^(?<name>[a-z0-9-]+)-(\\d+(?:\\.\\d+)+(?:-[a-zA-Z0-9]+)?)\\.tgz$";
//        Pattern pattern = Pattern.compile(regex);
//        Matcher matcher = pattern.matcher(packageName);
        setId(relativizePath);
//        if (matcher.find()) {
//            String name = matcher.group("name");
//            String version = matcher.group(2);
//            setVersion(version);
//        } else {
//           throw new RuntimeException("Invalid Helm package name format.");
//        }

    }

    public static HelmArtifactCoordinates parse(String relativizePath,String packageName) {
        log.info("parse helm relativizePath {}", relativizePath);
        return new HelmArtifactCoordinates(relativizePath,packageName);
    }

    @Override
    public String getId() {
        return getName();
    }

    public void setId(String id) {
        setCoordinate(NAME, id);
    }

    public void setVersion(String version) {
        super.setVersion(version);
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
