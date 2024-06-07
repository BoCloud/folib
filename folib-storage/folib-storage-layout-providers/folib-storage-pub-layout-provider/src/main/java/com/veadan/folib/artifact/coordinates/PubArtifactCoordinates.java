//package com.veadan.folib.artifact.coordinates;
//
//import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
//import com.veadan.folib.db.schema.Vertices;
//import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
//import org.neo4j.ogm.annotation.NodeEntity;
//import org.springframework.util.Assert;
//
//import javax.xml.bind.annotation.XmlAccessType;
//import javax.xml.bind.annotation.XmlAccessorType;
//import javax.xml.bind.annotation.XmlRootElement;
//import java.net.URI;
//import java.util.regex.Matcher;
//import java.util.regex.Pattern;
//
///**
// * This class is an {@link ArtifactCoordinates} implementation for pub
// * artifacts. <br>
// * See <a href="https://dart.cn/tools/pub/pubspec">Official pub pubspec
// * specification</a>.
// *
// * @author leipenghui
// */
//@NodeEntity(Vertices.PUB_ARTIFACT_COORDINATES)
//@XmlRootElement(name = "pubArtifactCoordinates")
//@XmlAccessorType(XmlAccessType.NONE)
//@ArtifactCoordinatesLayout(name = PubArtifactCoordinates.LAYOUT_NAME, alias = PubArtifactCoordinates.LAYOUT_ALIAS)
//public class PubArtifactCoordinates extends LayoutArtifactCoordinatesEntity<PubArtifactCoordinates, SemanticVersion> {
//
//    public static final String LAYOUT_NAME = "pub";
//
//    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
//
//    public static final String PUB_VERSION_REGEX = "(\\d+)\\.(\\d+)(?:\\.)?(\\d*)(\\.|-|\\+)?([0-9A-Za-z-.]*)?";
//
//    public static final String PUB_NAME_REGEX = "[a-z0-9_][\\w-.]*";
//
//    public static final String PUB_EXTENSION_REGEX = "(tar.gz)";
//
//    public static final String PUB_PACKAGE_PATH_REGEX = "(" + PUB_NAME_REGEX + ")/" + PUB_NAME_REGEX + "(-(" +
//            PUB_VERSION_REGEX + "))?\\." + PUB_EXTENSION_REGEX;
//
//    private static final Pattern PUB_NAME_PATTERN = Pattern.compile(PUB_NAME_REGEX);
//
//    private static final Pattern PUB_PATH_PATTERN = Pattern.compile(PUB_PACKAGE_PATH_REGEX);
//
//    private static final Pattern PUB_EXTENSION_PATTERN = Pattern.compile(PUB_EXTENSION_REGEX);
//
//    private static final String NAME = "name";
//
//    private static final String VERSION = "version";
//
//    private static final String EXTENSION = "extension";
//
//    public PubArtifactCoordinates() {
//        resetCoordinates(NAME, VERSION, EXTENSION);
//    }
//
//    public PubArtifactCoordinates(
//            String name,
//            String version,
//            String extension) {
//        setName(name);
//        setVersion(version);
//        setExtension(extension);
//    }
//
//    @ArtifactLayoutCoordinate
//    public String getName() {
//        return getCoordinate(NAME);
//    }
//
//    public void setName(String name) {
//        Matcher matcher = PUB_NAME_PATTERN.matcher(name);
//        Assert.isTrue(matcher.matches(),
//                String.format("The artifact's name [%s] should follow the PUB specification " +
//                                "(https://dart.cn/tools/pub/pubspec#name).",
//                        name));
//
//        setCoordinate(NAME, name);
//    }
//
//    @Override
//    public String getId() {
//        return getName();
//    }
//
//    public void setId(String id) {
//        setName(id);
//    }
//
//    @Override
//    public void setVersion(String version) {
//        SemanticVersion.parse(version);
//        super.setVersion(version);
//    }
//
//    public void setExtension(String extension) {
//        Matcher matcher = PUB_EXTENSION_PATTERN.matcher(extension);
//        Assert.isTrue(matcher.matches(), "Invalid artifact extension");
//
//        setCoordinate(EXTENSION, extension);
//    }
//
//    @ArtifactLayoutCoordinate
//    public String getExtension() {
//        return getCoordinate(EXTENSION);
//    }
//
//    @Override
//    public String convertToPath(PubArtifactCoordinates c) {
//        return String.format("%s/%s", c.getName(), getArtifactFileName());
//    }
//
//    @Override
//    public URI convertToResource(PubArtifactCoordinates c) {
//        return URI.create(String.format("%s/versions/%s", c.getName(), c.getArtifactFileName()));
//    }
//
//    public String getArtifactFileName() {
//        return String.format("%s-%s.%s", getName(), getVersion(), getExtension());
//    }
//
//    @Override
//    public SemanticVersion getNativeVersion() {
//        String versionLocal = getVersion();
//        if (versionLocal == null) {
//            return null;
//        }
//
//        try {
//            return SemanticVersion.parse(versionLocal);
//        } catch (IllegalArgumentException e) {
//            return null;
//        }
//    }
//
//    public static PubArtifactCoordinates parse(String path) {
//        Matcher matcher = PUB_PATH_PATTERN.matcher(path);
//
//        Assert.isTrue(matcher.matches(),
//                String.format("Illegal artifact path [%s], PUB artifact path should be in the form of " +
//                                "'/{artifactName}/{artifactFile}'.",
//                        path));
//
//        String name = matcher.group(1);
//        String version = matcher.group(2);
//        String extension = matcher.group(14);
//        return new PubArtifactCoordinates(name, version, extension);
//    }
//
//    public static PubArtifactCoordinates of(String name, String version, String packagingSuffixes) {
//        return new PubArtifactCoordinates(name, version, packagingSuffixes);
//    }
//
//}
