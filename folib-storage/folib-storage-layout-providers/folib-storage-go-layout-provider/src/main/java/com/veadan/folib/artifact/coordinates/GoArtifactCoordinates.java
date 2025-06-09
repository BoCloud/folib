package com.veadan.folib.artifact.coordinates;

import cn.hutool.core.io.FileUtil;
import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:36
 */
@NodeEntity(Vertices.GO_ARTIFACT_COORDINATES)
@XmlRootElement(name = "goArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = GoArtifactCoordinates.LAYOUT_NAME, alias = GoArtifactCoordinates.LAYOUT_ALIAS)
public class GoArtifactCoordinates extends LayoutArtifactCoordinatesEntity<GoArtifactCoordinates, SemanticVersion> {
    public static final String LAYOUT_NAME = "go";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final Logger logger = LoggerFactory.getLogger(GoArtifactCoordinates.class);


    public static final String NAME = "name";
    public static final String VERSION = "version";
    public static final String EXTENSION = "extension";

    public static final String GO_EXTENSION_REGEX = "(zip)";
    private static final Pattern GO_EXTENSION_PATTERN = Pattern.compile(GO_EXTENSION_REGEX);

    public GoArtifactCoordinates() {
        resetCoordinates(NAME, EXTENSION, VERSION);
    }

    public GoArtifactCoordinates(String name, String extension, String version) {
        this();
        setName(name);
        setExtension(extension);
        setVersion(version);
    }


    public static GoArtifactCoordinates parse(String path) {
        String[] parts = path.split("/@");

        Assert.isTrue(parts.length == 2, String.format("Illegal artifact path [%s]", path));

        String modulePath = parts[0];
        String after = parts[1];

        Assert.isTrue(after.startsWith("v/"), String.format("Illegal artifact path [%s]", path));
        // remove 'v/'
        after = after.substring(2);

        String extName = FileUtil.extName(after);
        String suffix = "." + extName;

        Assert.isTrue(Objects.equals(".zip", suffix), String.format("Illegal artifact path [%s]", path));

        String version = after.substring(0, after.lastIndexOf(suffix));

        return new GoArtifactCoordinates(modulePath, extName, version);
    }

    @Override
    public String getId() {
        return getName();
    }

    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }

    public void setName(String name) {
        setCoordinate(NAME, name);
    }

    @ArtifactLayoutCoordinate
    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    private void setExtension(String extension) {
        Matcher matcher = GO_EXTENSION_PATTERN.matcher(extension);
        Assert.isTrue(matcher.matches(), "Invalid artifact extension:" + extension);

        setCoordinate(EXTENSION, extension);
    }

    @Override
    public void setVersion(String version) {
        super.setVersion(version);
    }

    @Override
    public SemanticVersion getNativeVersion() {
        return null;
    }

    @Override
    public String convertToPath(GoArtifactCoordinates artifactCoordinates) {
        return String.format("%s/@v/%s.%s", artifactCoordinates.getName(), artifactCoordinates.getVersion(), artifactCoordinates.getExtension());
    }

}
