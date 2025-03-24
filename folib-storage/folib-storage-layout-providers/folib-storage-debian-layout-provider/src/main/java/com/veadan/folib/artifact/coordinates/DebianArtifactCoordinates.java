package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import lombok.extern.slf4j.Slf4j;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.regex.Matcher;

/**
 * @author huayanjun
 * <p>
 * 1.下载包索引文件 /dists/{distribution}/{component}/binary-{architecture}/Packages.gz
 * 2.下载包 路径   /pool/[component]/[first-letter]/[package-name]/[package-filename]
 */

@Slf4j
@NodeEntity(Vertices.DEBIAN_ARTIFACT_COORDINATES)
@XmlRootElement(name = "debianArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = DebianConstant.LAYOUT_NAME, alias = DebianConstant.LAYOUT_ALIAS)
public class DebianArtifactCoordinates
        extends LayoutArtifactCoordinatesEntity<DebianArtifactCoordinates, SemanticVersion> {

    public DebianArtifactCoordinates() {
        resetCoordinates(DebianConstant.ARCHITECTURE, DebianConstant.COMPONENT, DebianConstant.DISTRIBUTION);
    }

    public DebianArtifactCoordinates(String component,  String name, String extension) {
        setName(name);
        if(component!=null){
            setComponent(component);
        }
        setExtension(extension);
    }

    public void setDistribution(String distribution) {
        setCoordinate(DebianConstant.DISTRIBUTION, distribution);
    }
    public String getDistribution() {
        return getCoordinate(DebianConstant.DISTRIBUTION);
    }
    public void setComponent(String component) {
        setCoordinate(DebianConstant.COMPONENT, component);
    }
    public String getComponent() {
        return getCoordinate(DebianConstant.COMPONENT);
    }
    public void setArchitecture(String architecture) {
        setCoordinate(DebianConstant.ARCHITECTURE, architecture);
    }
    public String getArchitecture() {
        return getCoordinate(DebianConstant.ARCHITECTURE);
    }


    public void setExtension(String extension) {
        setCoordinate(DebianConstant.EXTENSION, extension);
    }

    public String getExtension() {
        return getCoordinate(DebianConstant.EXTENSION);
    }

    public void setName(String name) {
        setCoordinate(DebianConstant.NAME, name);
    }

    public String getName() {
        return getCoordinate(DebianConstant.NAME);
    }

    public void setFileName(String fileName) {
        setCoordinate(DebianConstant.FILENAME, fileName);
    }

    public String getFileName() {
        return getCoordinate(DebianConstant.FILENAME);
    }

    @Override
    public String getId() {
        return getCoordinate(DebianConstant.NAME);
    }

    @Override
    public SemanticVersion getNativeVersion() {
        String versionLocal = getVersion();
        if (versionLocal == null) {
            return null;
        }
        try {
            return SemanticVersion.parse(versionLocal);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    @Override
    public String convertToPath(DebianArtifactCoordinates c) {
        String path = null;
        //没有发行版是具体的包
        if (DebianConstant.DEFAULT_EXTENSION.equals(c.getExtension())) {
            path=c.getName();
//            if(c.getComponent()==null){
//                path=c.getName();
//            }else {
//                String subName = getLibPackagePath(c.getFileName());
//                path = String.format("%s/%s/%s/%s/%s", DebianConstant.DEB_PREFIX, c.getComponent(), subName, c.getFileName(), c.getName());
//            }

        } else if (DebianConstant.PACKAGE_EXTENSION.equals(c.getExtension())) {
            path = String.format("%s/%s/%s/binary-%s/%s", DebianConstant.PACKAGE_PREFIX, getDistribution(), getComponent(), getArchitecture(), getName());
        }
        return path;
    }

    private String getLibPackagePath(String baseName) {
        if (!baseName.startsWith("lib")) {
            return baseName.charAt(0) + "";
        }
        if (baseName.length() <= 4) {
            return "lib";
        }
        return baseName.substring(0, 4);
    }

    public static DebianArtifactCoordinates parse(String path) {
        DebianArtifactCoordinates coordinates;
        if (path.startsWith("pool")) {
            Matcher matcher = DebianConstant.PATH_PATTERN.matcher(path);
            if(matcher.matches()) {
                String component = matcher.group(1);
                String fileName = matcher.group(2);
                String version = matcher.group(3);
                log.info("path is component:{},filename:{},version:{}", component, fileName, version);
                coordinates = DebianArtifactCoordinates.of(null, path, DebianConstant.DEFAULT_EXTENSION);
                coordinates.setVersion(version);
                coordinates.setFileName(fileName);
            }else {
                String[] split = path.split("/");
                String component = "main";
                if(split.length >1) {
                    component=split[1];
                }
                String name = path.substring(path.lastIndexOf('/') + 1);
                coordinates = DebianArtifactCoordinates.of(null, path, DebianConstant.DEFAULT_EXTENSION);
                coordinates.setVersion("1.0.0");
                coordinates.setFileName(name);

            }

        } else if(path.endsWith(DebianConstant.DEFAULT_EXTENSION)) {
            Matcher matcher = DebianConstant.CUSTOM_PATTERN.matcher(path);
            Assert.isTrue(matcher.matches(), "Invalid debian package path");
            String packageName = matcher.group(1) != null ? matcher.group(1) : matcher.group(5);
            String version = matcher.group(2) != null ? matcher.group(2) : matcher.group(6);
            String architecture = matcher.group(3) != null ? matcher.group(3) : matcher.group(7);
            String extension = matcher.group(4) != null ? matcher.group(4) : matcher.group(8);
            coordinates = new DebianArtifactCoordinates();
            coordinates.setFileName(packageName);
            coordinates.setVersion(version);
            coordinates.setArchitecture(architecture);
            coordinates.setExtension(extension);
            coordinates.setName(path);
        }else {
            Matcher matcher = DebianConstant.PACKAGE_PATTERN.matcher(path);
            Assert.isTrue(matcher.matches(), "Invalid debian package path"+path);
            String codename = matcher.group("codename");
            String component = matcher.group("component");
            String architecture = matcher.group("architecture");
            String name = matcher.group("filename");

            coordinates = DebianArtifactCoordinates.of(component, name, DebianConstant.PACKAGE_EXTENSION);
            coordinates.setDistribution(codename);
            coordinates.setArchitecture(architecture);
            coordinates.setVersion("1.0.0");
        }
        return coordinates;
    }

    public static DebianArtifactCoordinates of(String component, String name, String extension) {
        return new DebianArtifactCoordinates(component, name, extension);
    }
}
