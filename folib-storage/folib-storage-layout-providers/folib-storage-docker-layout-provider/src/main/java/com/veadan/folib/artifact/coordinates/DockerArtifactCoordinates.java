package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@Slf4j
@NodeEntity(Vertices.DOCKER_ARTIFACT_COORDINATES)
@XmlRootElement(name = "DockerArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = DockerArtifactCoordinates.LAYOUT_NAME, alias = DockerArtifactCoordinates.LAYOUT_ALIAS)
public class DockerArtifactCoordinates
        extends LayoutArtifactCoordinatesEntity<DockerArtifactCoordinates, String> {

    public static final String LAYOUT_NAME = "Docker";

    public static final String LAYOUT_ALIAS = "Docker";

    //public static final String REPOSITORY = "repository";
    public static final String IMAGE_NAME = "name";

    public static final String TAG = "tag";

    public static final String LAYERS = "layers";

    public static final String ARTIFACT_PATH = "path";

    public static final String SHA_256 = "sha256";

    public static final String CHECKSUM_SHA_256 = ".sha256";

    public static final String SELF_METADATA = ".metadata";

    public static final String FO_LIBRARY_METADATA = ".foLibrary-metadata";


    public DockerArtifactCoordinates() {
        resetCoordinates(LAYERS, ARTIFACT_PATH);
    }


    //
    // TODO: We will have to think about something like this:
    //


    public DockerArtifactCoordinates(String repository,
                                     String reference) {
        // if any of the required arguments are empty, throw an error
        if (StringUtils.isBlank(repository)) {
            throw new IllegalArgumentException("The repository field is mandatory.");
        }

        if (StringUtils.isBlank(reference)) {
            throw new IllegalArgumentException("The reference field is mandatory.");
        }

        setId(repository);
        setVersion(reference);

        // TODO:
        // setLayers(layers);
    }

    public DockerArtifactCoordinates(String repository,
                                     String reference,
                                     String layers,
                                     String artifactPath) {
        // if any of the required arguments are empty, throw an error
        if (StringUtils.isBlank(repository)) {
            throw new IllegalArgumentException("The repository field is mandatory.");
        }

//        if (StringUtils.isBlank(reference))
//        {
//            throw new IllegalArgumentException("The reference field is mandatory.");
//        }

        setId(repository);
        setVersion(reference);
        setTAG(reference);
        setLayers(layers);
        setArtifactPath(artifactPath);

        // TODO:
        // setLayers(layers);
    }

    // todo 优化
    public static DockerArtifactCoordinates parse(String path) {
        // TODO:
        if (Objects.isNull(path)) {
            return null;
        }
        String tag = null;
        String[] strings = null;
        strings = path.split("/");
        if (!path.contains("/blobs/")) {
            tag = strings[strings.length - 2];
        } else {
            tag = "v2";
        }

        String repository = strings[0];
        String artifactPath = "";
        String layers = "";
        if (strings[strings.length - 1].contains("sha256:")) {
            layers = strings[strings.length - 1];
            String finalLayers = layers;
            artifactPath = Arrays.stream(strings).filter(data -> !Objects.equals(finalLayers, data) || !Objects.equals(repository, data))
                    .collect(Collectors.joining("/"));

        } else if (strings[strings.length - 1].contains("manifest.json")) {
            layers = strings[strings.length - 1];
            String finalLayers = layers;
            artifactPath = Arrays.stream(strings).filter(data -> !Objects.equals(finalLayers, data) || !Objects.equals(repository, data))
                    .collect(Collectors.joining("/"));
        }
        if (StringUtils.isBlank(artifactPath)) {
            throw new IllegalArgumentException(String.format("Path [%s] not a standard Docker layout file", path));
        }
        return new DockerArtifactCoordinates(repository, tag, layers, artifactPath);
    }

    public String getIMAGE_NAME() {
        String str = getArtifactPath().replace("/" + getLayers(), "");
        str = str.replace("/", ":");
        return str;
    }

    public void setIMAGE_NAME(String imageName) {
        setCoordinate(IMAGE_NAME, imageName);
    }

    @Override
    @XmlAttribute(name = "imageName")
    public String getId() {
        return getIMAGE_NAME();
    }


    public void setId(String id) {
        setIMAGE_NAME(id);
    }

    @Override
    @XmlAttribute(name = "version")
    public String getVersion() {
        return super.getVersion();
    }

    @Override
    @XmlAttribute(name = "path")
    public String getPath() {
        return super.getPath();
    }

    @Override
    public void setVersion(String version) {
        //setCoordinate(TAG,version);
        super.setVersion(version);
    }


    /**
     * @return Returns the reconstructed path from the stored coordinate values
     */

    public String toPath() {
        // TODO:
        return ARTIFACT_PATH;
    }

    /**
     * @return Returns the native version of the package
     */
    @Override
    public String getNativeVersion() {
        return getVersion();
    }

    /**
     * @return Returns a map data structure of the coordinates without the TAG coordinate
     */

    public Map<String, String> dropVersion() {
        Map<String, String> result = getCoordinates();
        result.remove(super.getVersion());

        return result;
    }

    @Override
    public GenericArtifactCoordinates getHierarchyChild() {
        return super.getHierarchyChild();
    }

    @Override
    public void setHierarchyChild(GenericArtifactCoordinates node) {
        super.setHierarchyChild(node);
    }

    @Override
    public String convertToPath(DockerArtifactCoordinates artifactCoordinates) {
        return artifactCoordinates.getArtifactPath();
        // return new  DockerArtifactGenerator(artifactCoordinates.getPath()).getImageManifestPath().toString();
        //  return String.format("%s/%s/%s/%s", artifactCoordinates.g, c.getName(), c.getVersion(), c.getArtifactFileName());
    }

    @Override
    public URI convertToResource(DockerArtifactCoordinates artifactCoordinates) {
        return super.convertToResource(artifactCoordinates);
    }


    @Override
    public void setUuid(String uuid) {
        super.setUuid(uuid);
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "tag")
    public String getTAG() {
        return getCoordinate(TAG);
    }

    public void setTAG(String tag) {
        setCoordinate(TAG, tag);
    }

    @ArtifactLayoutCoordinate
    public String getLayers() {
        return getCoordinate(LAYERS);
    }

    public void setLayers(String layers) {
        setCoordinate(LAYERS, layers);
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "artifactPath")
    public String getArtifactPath() {
        return getCoordinate(ARTIFACT_PATH);
    }

    public void setArtifactPath(String artifactPath) {
        setCoordinate(ARTIFACT_PATH, artifactPath);
    }

    public String getName() {
        return getCoordinate("name");
    }

    public static boolean isManifestPath(Path path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || Files.isDirectory(path) || Files.isHidden(path)) {
                return false;
            }
            String name = path.getFileName().toString();
            return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && !path.toString().contains("blobs/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean isManifestPath(String name) {
        if (StringUtils.isBlank(name)) {
            return false;
        }
        return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && !name.contains("blobs");
    }

    public static boolean isDockerVersion(RepositoryPath path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || Files.isHidden(path)) {
                return false;
            }
            String fullPath = path.toString();
            String relativizePath = RepositoryFiles.relativizePath(path);
            int deepSize = relativizePath.split("/").length;
            int two = 2;
            if (deepSize < two) {
                return false;
            }
            if (Files.isDirectory(path)) {
                return deepSize == two && !fullPath.contains("blobs") && !fullPath.contains("manifest");
            }
            String name = path.getFileName().toString();
            return name.startsWith(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA) && !fullPath.contains("blobs/sha256") && !fullPath.contains("manifest/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean exclude(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        return name.endsWith(CHECKSUM_SHA_256) || name.endsWith(SELF_METADATA) || !name.endsWith(FO_LIBRARY_METADATA);
    }

    public static boolean include(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        return name.contains(SHA_256) && !name.endsWith(CHECKSUM_SHA_256) && !name.endsWith(SELF_METADATA) && !name.endsWith(FO_LIBRARY_METADATA);
    }

}
