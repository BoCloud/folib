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

    public HelmArtifactCoordinates() {
    }

    public HelmArtifactCoordinates(String baseName) {
        this.BASE_NAME = baseName;
    }

    public HelmArtifactCoordinates(String baseName, String packageType, String metaData, String artifactSuffix) {
        this.BASE_NAME = baseName;
        this.PACKAGE_TYPE = packageType;
        this.META_DATA = metaData;
        this.ARTIFACT_SUFFIX = artifactSuffix;
    }

    public static HelmArtifactCoordinates parse(String relativizePath) {
        log.info("parse helm relativizePath {}", relativizePath);
        return new HelmArtifactCoordinates(relativizePath);
    }

    @Override
    public String getId() {
        return BASE_NAME; //todo Chart id
    }

    public void setId(String id) {
        setCoordinate(BASE_NAME, id);
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
    public String convertToPath(HelmArtifactCoordinates artifactCoordinates) {
        return artifactCoordinates.getBASE_NAME();
    }


}
