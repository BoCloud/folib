package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


@Slf4j
@NodeEntity(Vertices.HUGGINGFACE_ARTIFACT_COORDINATES)
@XmlRootElement(name = "HuggingFaceArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = HuggingFaceArtifactCoordinates.LAYOUT_NAME, alias = HuggingFaceArtifactCoordinates.LAYOUT_ALIAS)
public class HuggingFaceArtifactCoordinates extends LayoutArtifactCoordinatesEntity<HuggingFaceArtifactCoordinates, HuggingFaceArtifactCoordinates> {

    public static final String LAYOUT_NAME = "HuggingFace";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    private static final String SCOPE = "scope";
    private static final String NAME = "name";

    private static final String VERSION = "version";
    private static final String TIMESTAMP = "timestamp";
    private static final String FILENAME = "filename";
    private static final String EXTENSION = "extension";
    public static final String PATH_ORG_REGEX = "models/([^/]+)/([^/]+)/([^/]+)/(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z)/([^/]+(?:/[^/]+)*)";
    public static final String PATH_REGEX = "models/([^/]+)/([^/]+)/(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z)/([^/]+(?:/[^/]+)*)";
    public final String suffix = "\\.(\\w+)$";

    private static final Pattern HFML_ORG_PATH_PATTERN = Pattern.compile(PATH_ORG_REGEX);

    private static final Pattern HFML_PATH_PATTERN = Pattern.compile(PATH_REGEX);


    public HuggingFaceArtifactCoordinates() {
        resetCoordinates(PATH, SCOPE, NAME, VERSION, TIMESTAMP, FILENAME, EXTENSION);
    }

    public HuggingFaceArtifactCoordinates(String path) {
        if (HFML_ORG_PATH_PATTERN.matcher(path).matches()) {
            isOrg(path);
        } else if (HFML_PATH_PATTERN.matcher(path).matches()) {
            isModel(path);
        }else {
            setCoordinate(PATH, path);
        }
    }

    public void isOrg(String path) {

        Matcher matcher = HFML_ORG_PATH_PATTERN.matcher(path);
        String msg = String.format("Illegal artifact path [%s], HuggingFaceML artifact path should be in the form of " +
                "'models/{organization}/{modelName}/{revision}/{timestamp}/{filename:.+}'.", path);
        Assert.isTrue(matcher.matches(), msg);
        String spope = matcher.group(1);
        String name = matcher.group(2);
        String version = matcher.group(3);
        String timestamp = matcher.group(4);

        String filename = Paths.get(path).getFileName().toString();

        Matcher matcher1 = Pattern.compile(suffix).matcher(filename);
        String extension = null;
        if (matcher1.find()) {
            extension = matcher1.group(1);
        }

        if (StringUtils.isBlank(name) || StringUtils.isBlank(version) || StringUtils.isBlank(extension)) {
            log.warn("Path [{}] name [{}] version [{}] extension [{}] pattern [{}] parse error", path, name, version, extension, HFML_PATH_PATTERN.toString());
            throw new RuntimeException(msg);
        }

        setCoordinate(PATH, path);
        setCoordinate(SCOPE, spope);
        super.setVersion(version);
        setCoordinate(FILENAME, filename);
        setCoordinate(EXTENSION, extension);
        setCoordinate(TIMESTAMP, timestamp);
    }

    public void isModel(String path) {

        Matcher matcher = HFML_PATH_PATTERN.matcher(path);
        String msg = String.format("Illegal artifact path [%s], HuggingFaceML artifact path should be in the form of " +
                "'models/{modelName}/{revision}/{timestamp}/{filename:.+}'.", path);
        Assert.isTrue(matcher.matches(), msg);

        String name = matcher.group(1);
        String version = matcher.group(2);
        String timestamp = matcher.group(3);
        String filename = Paths.get(path).getFileName().toString();
        Matcher matcher1 = Pattern.compile(suffix).matcher(filename);
        String extension = null;
        if (matcher1.find()) {
            extension = matcher1.group(1);
        }

        if (StringUtils.isBlank(name) || StringUtils.isBlank(version) || StringUtils.isBlank(extension)) {
            log.warn("Path [{}] name [{}] version [{}] extension [{}] pattern [{}] parse error", path, name, version, extension, HFML_PATH_PATTERN.toString());
            throw new RuntimeException(msg);
        }
        setCoordinate(PATH, path);
        super.setVersion(version);
        setCoordinate(FILENAME, filename);
        setCoordinate(EXTENSION, extension);
        setCoordinate(TIMESTAMP, timestamp);
    }

    public String getId() {
        return getCoordinate(PATH);
    }

    public void setId(String id) {
        setCoordinate(PATH, id);
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "path")
    public String getPath() {
        return getCoordinate(PATH);
    }

    @Override
    public HuggingFaceArtifactCoordinates getNativeVersion() {
        return null;
    }

    @Override
    public String convertToPath(HuggingFaceArtifactCoordinates artifactCoordinates) {
        return artifactCoordinates.getId();
    }
}
