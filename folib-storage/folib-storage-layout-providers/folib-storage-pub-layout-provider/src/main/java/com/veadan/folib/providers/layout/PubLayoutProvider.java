package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.PubRepositoryFeatures;
import com.veadan.folib.repository.PubRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author leipenghui
 */
@Component
public class PubLayoutProvider
        extends AbstractLayoutProvider<PubArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(PubLayoutProvider.class);

    public static final String ALIAS = PubArtifactCoordinates.LAYOUT_NAME;

    public static final String PUB_USER_PATH = "-/user/org.couchdb.user:";

    public static final String PACKAGE_JSON = "package.json";

    public static final String OH_PACKAGE_JSON = "oh-package.json5";

    public static final String DEFAULT_PACKAGE_JSON_PATH = "package/package.json";
    public static final String OHPM_PACKAGE_JSON_PATH = "package/oh-package.json5";
    public static final String DEFAULT_SUFFIX = "tgz";

    public static final Pattern PUB_URL_USERNAME_PATTERN = Pattern.compile(
            "(?:" + PubLayoutProvider.PUB_USER_PATH + ")(.*)");

    @Inject
    private PubRepositoryManagementStrategy pubRepositoryManagementStrategy;

    @Inject
    private PubRepositoryFeatures pubRepositoryFeatures;


    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public PubArtifactCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException {
        return PubArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return path.getFileName().toString().endsWith(PACKAGE_JSON);
    }

    public boolean isPubMetadata(RepositoryPath path) {
        return path.getFileName().toString().endsWith("pubspec.lock");
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && !isPubMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isPubMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                default:

                    break;
            }
        }

        return result;
    }

    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return pubRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return pubRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, MessageDigestAlgorithms.SHA_512)
                .collect(Collectors.toSet());
    }

}
