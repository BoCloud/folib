package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.NpmLayoutProvider;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.net.URI;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class is an {@link ArtifactCoordinates} implementation for npm
 * artifacts. <br>
 * See <a href="https://docs.npmjs.com/files/package.json">Official npm package
 * specification</a>.
 *
 * @author xuxinping
 */
@NodeEntity(Vertices.NPM_ARTIFACT_COORDINATES)
@XmlRootElement(name = "npmArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@ArtifactCoordinatesLayout(name = NpmArtifactCoordinates.LAYOUT_NAME, alias = NpmArtifactCoordinates.LAYOUT_ALIAS)
public class NpmArtifactCoordinates extends LayoutArtifactCoordinatesEntity<NpmArtifactCoordinates, SemanticVersion> {

    public static final String LAYOUT_NAME = "npm";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;

    public static final String NPM_BINARY_PATH_PREFIX = "-/binary/";

    public static final String NPM_VERSION_REGEX = "(\\d+)\\.(\\d+)(?:\\.)?(\\d*)(\\.|-|\\+)?([0-9A-Za-z-.]*)?";

    public static final String NPM_NAME_REGEX = "[a-zA-Z0-9][\\w-.]*";

    public static final String NPM_EXTENSION_REGEX = "(tgz|json|har|json5)";

    public static final String NPM_PACKAGE_PATH_REGEX = "(?:(@?" + NPM_NAME_REGEX + ")/)?(" + NPM_NAME_REGEX + ")/(" +
            NPM_VERSION_REGEX + ")/" + NPM_NAME_REGEX + "(-(" +
            NPM_VERSION_REGEX + "))?\\." + NPM_EXTENSION_REGEX;

    private static final Pattern NPM_NAME_PATTERN = Pattern.compile(NPM_NAME_REGEX);

    private static final Pattern NPM_PATH_PATTERN = Pattern.compile(NPM_PACKAGE_PATH_REGEX);

    private static final Pattern NPM_EXTENSION_PATTERN = Pattern.compile(NPM_EXTENSION_REGEX);

    private static final String SCOPE = "scope";

    private static final String NAME = "name";

    private static final String VERSION = "version";

    private static final String EXTENSION = "extension";

    private static final String DISTRIBUTION = "distribution";

    public NpmArtifactCoordinates() {
        resetCoordinates(SCOPE, NAME, VERSION, EXTENSION);
    }

    public NpmArtifactCoordinates(String scope,
                                  String name,
                                  String version,
                                  String extension) {
        setScope(scope);
        setName(name);
        setVersion(version);
        setExtension(extension);
    }

    public NpmArtifactCoordinates(String scope,
                                  String name,
                                  String version,
                                  String extension,
                                  String distribution) {
        if (StringUtils.isNotBlank(name)) {
            setCoordinate(NAME, name);
        }
        if (StringUtils.isNotBlank(version)) {
            super.setVersion(version);
        }
        if (StringUtils.isNotBlank(extension)) {
            setCoordinate(EXTENSION, extension);
        }
        setDistribution(distribution);
    }

    @ArtifactLayoutCoordinate
    public String getScope() {
        return getCoordinate(SCOPE);
    }

    public void setScope(String scope) {
        if (StringUtils.isBlank(scope)) {
            return;
        }
        Assert.isTrue(scope.startsWith("@"), "Scope should starts with '@'.");
        setCoordinate(SCOPE, scope);
    }

    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }

    public void setName(String name) {
        Matcher matcher = NPM_NAME_PATTERN.matcher(name);
        Assert.isTrue(matcher.matches(),
                String.format("The artifact's name [%s] should follow the NPM specification " +
                                "(https://docs.npmjs.com/files/package.json#name).",
                        name));

        setCoordinate(NAME, name);
    }

    @Override
    public String getId() {
        if (getScope() == null) {
            return getName();
        }

        return String.format("%s/%s", getScope(), getName());
    }

    public void setId(String id) {
        setName(id);
    }

    @Override
    public void setVersion(String version) {
        SemanticVersion.parse(version);
        super.setVersion(version);
    }

    public void setExtension(String extension) {
        Matcher matcher = NPM_EXTENSION_PATTERN.matcher(extension);
        Assert.isTrue(matcher.matches(), "Invalid artifact extension");

        setCoordinate(EXTENSION, extension);
    }

    @ArtifactLayoutCoordinate
    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    @ArtifactLayoutCoordinate
    public String getDistribution() {
        return getCoordinate(DISTRIBUTION);
    }

    public void setDistribution(String distribution) {
        setCoordinate(DISTRIBUTION, distribution);
    }

    @Override
    public String convertToPath(NpmArtifactCoordinates c) {
        if (StringUtils.isNotBlank(c.getDistribution())) {
            return c.getDistribution();
        }
        String group = c.getGroup();
        if (StringUtils.isNotBlank(group)) {
            return String.format("%s/%s/%s/%s", group, c.getName(), c.getVersion(), c.getArtifactFileName());
        }
        return String.format("%s/%s/%s", c.getName(), c.getVersion(), c.getArtifactFileName());
    }

    @Override
    public URI convertToResource(NpmArtifactCoordinates c) {
        if (StringUtils.isNotBlank(c.getDistribution())) {
            return URI.create(c.getDistribution());
        }
        String path = convertToPath(c);
        if (path.endsWith(NpmLayoutProvider.PACKAGE_JSON)) {
            return URI.create(String.format("%s/-/%s-%s.%s", c.getId(), "package", c.getVersion(), "json"));
        } else if (path.endsWith(NpmLayoutProvider.OH_PACKAGE_JSON)) {
            return URI.create(String.format("%s/-/%s-%s.%s", c.getId(), "oh-package", c.getVersion(), "json5"));
        }
        return URI.create(String.format("%s/-/%s", c.getId(), c.getArtifactFileName()));
    }

    public String getGroup() {
        String scopeLocal = getScope();
        String nameLocal = getName();

        return scopeLocal == null ? "" : scopeLocal;
    }

    public String getArtifactFileName() {
        if ("json".equals(getExtension())) {
            return "package.json";
        } else if ("json5".equals(getExtension())) {
            return "oh-package.json5";
        }
        return String.format("%s-%s.%s", getName(), getVersion(), getExtension());
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

    public static NpmArtifactCoordinates parse(String path) {
        if (path.startsWith(NPM_BINARY_PATH_PREFIX)) {
            //binary
            String artifactPath = path.substring(NPM_BINARY_PATH_PREFIX.length());
            String[] arr = artifactPath.split(GlobalConstants.SEPARATOR);
            String name = arr[0];
            String version = arr[1];
            String extension = FilenameUtils.getExtension(path);
            return new NpmArtifactCoordinates(null, name, version, extension, path);
        }
        Matcher matcher = NPM_PATH_PATTERN.matcher(path);

        Assert.isTrue(matcher.matches(),
                String.format("Illegal artifact path [%s], NPM artifact path should be in the form of " +
                                "'{artifactGroup}/{artifactName}/{artifactVersion}/{artifactFile}'.",
                        path));

        String group = matcher.group(1);
        String name = matcher.group(2);
        String version = matcher.group(3);
        String extension = matcher.group(16);

        if (StringUtils.isNotBlank(group) && group.startsWith("@")) {
            return new NpmArtifactCoordinates(group, name, version, extension);
        }

        return new NpmArtifactCoordinates(null, name, version, extension);
    }

    public static NpmArtifactCoordinates of(String packageId,
                                            String version, String packagingSuffixes) {
        if (packageId.contains("/")) {
            String[] nameSplit = packageId.split("/");

            return new NpmArtifactCoordinates(nameSplit[0], nameSplit[1], version, packagingSuffixes);
        }

        return new NpmArtifactCoordinates(null, packageId, version, packagingSuffixes);
    }

    public static String calculatePackageId(String packageScope, String packageName) {
        return packageScope == null ? packageName : String.format("%s/%s", packageScope, packageName);
    }

    public static NpmArtifactCoordinates resolveName(RepositoryPath repositoryPath, String artifactPath) {
        NpmArtifactCoordinates npmArtifactCoordinates = new NpmArtifactCoordinates();
        String[] arr = artifactPath.split(GlobalConstants.SEPARATOR);
        if (!arr[0].startsWith(GlobalConstants.AT)) {
            npmArtifactCoordinates.setName(arr[0]);
        } else {
            npmArtifactCoordinates.setScope(arr[0]);
            if (arr.length >= 2) {
                npmArtifactCoordinates.setName(arr[1]);
            }
        }
        return npmArtifactCoordinates;
    }

}
