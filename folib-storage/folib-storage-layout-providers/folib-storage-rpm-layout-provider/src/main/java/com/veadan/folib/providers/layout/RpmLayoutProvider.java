package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.RpmArtifactCoordinates;
import com.veadan.folib.providers.header.HeaderMappingRegistry;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.repository.RpmRepositoryFeatures;
import com.veadan.folib.repository.RpmRepositoryManagementStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 *
 */
@Component
public class RpmLayoutProvider extends AbstractLayoutProvider<RpmArtifactCoordinates>{

    private static final Logger logger = LoggerFactory.getLogger(RpmLayoutProvider.class);

    public static final String ALIAS = RpmArtifactCoordinates.LAYOUT_NAME;

    public static final String USER_AGENT_PREFIX =ALIAS;

//    public static final String RPM_USER_PATH = "-/user/org.couchdb.user:";

//    public static final Pattern RPM_URL_USERNAME_PATTERN = Pattern.compile(
//            "(?:" + RpmLayoutProvider.RPM_USER_PATH + ")(.*)");


    @Inject
    private RpmRepositoryManagementStrategy rpmRepositoryManagementStrategy;

    @Inject
    private RpmRepositoryFeatures rpmRepositoryFeatures;

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    @PostConstruct
    public void register()
    {
       // headerMappingRegistry.register(ALIAS, USER_AGENT_PREFIX);
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public RpmArtifactCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException    {
        return RpmArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }


    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {return false;}

    public boolean isRpmMetadata(RepositoryPath path)
    {
        return !path.getFileName().toString().endsWith(".rpm");
       // return true;
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
                    value = (Boolean) value && !isRpmMetadata(repositoryPath);

                    if (value != null)
                    {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isRpmMetadata(repositoryPath);

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
        return rpmRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return rpmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

}
