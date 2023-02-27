package com.veadan.folib.providers.layout;

import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.NpmRepositoryFeatures;
import com.veadan.folib.repository.NpmRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;

import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author @author veadan
 */
@Component
public class NpmLayoutProvider
        extends AbstractLayoutProvider<NpmArtifactCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(NpmLayoutProvider.class);

    public static final String ALIAS = NpmArtifactCoordinates.LAYOUT_NAME;

    public static final String NPM_USER_PATH = "-/user/org.couchdb.user:";

    public static final String  PACKAGE_JSON = "package.json";

    public static final Pattern NPM_URL_USERNAME_PATTERN = Pattern.compile(
            "(?:" + NpmLayoutProvider.NPM_USER_PATH + ")(.*)");

    @Inject
    private NpmRepositoryManagementStrategy npmRepositoryManagementStrategy;

    @Inject
    private NpmRepositoryFeatures npmRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    protected NpmArtifactCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException
    {
        return NpmArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return path.getFileName().toString().endsWith(PACKAGE_JSON);
    }

    public boolean isNpmMetadata(RepositoryPath path)
    {
        return path.getFileName().toString().endsWith("package-lock.json") ||
               path.getFileName().toString().endsWith("npm-shrinkwrap.json");
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException
    {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                                                                                            attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes)
        {
            Object value = result.get(attributeType);
            switch (attributeType)
            {
                case ARTIFACT:
                    value = (Boolean) value && !isNpmMetadata(repositoryPath);

                    if (value != null)
                    {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isNpmMetadata(repositoryPath);

                    if (value != null)
                    {
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
    public RepositoryManagementStrategy getRepositoryManagementStrategy()
    {
        return npmRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return npmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet()
    {
        return Stream.of(MessageDigestAlgorithms.SHA_1)
                     .collect(Collectors.toSet());
    }

}
