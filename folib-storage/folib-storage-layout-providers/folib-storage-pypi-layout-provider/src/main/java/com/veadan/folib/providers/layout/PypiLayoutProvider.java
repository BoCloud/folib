package com.veadan.folib.providers.layout;

import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.PypiRepositoryFeatures;
import com.veadan.folib.repository.PypiRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class PypiLayoutProvider
        extends AbstractLayoutProvider<PypiArtifactCoordinates>
{
    private static final Logger logger = LoggerFactory.getLogger(PypiLayoutProvider.class);

    public static final String ALIAS = PypiArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private PypiRepositoryManagementStrategy pypiRepositoryManagementStrategy;

    @Inject
    private PypiRepositoryFeatures pypiRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS );
    }

    @Override
    public PypiArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return PypiArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        // TODO: Fix
        return false;
    }

    public boolean isMetadata(RepositoryPath path)
    {
        // TODO: Fix
        return false;
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
                    value = (Boolean) value && !isMetadata(repositoryPath);
    
                    if (value != null)
                    {
                        result.put(attributeType, value);
                    }
    
                    break;
                case METADATA:
                    value = (Boolean) value || isMetadata(repositoryPath);
    
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
        return pypiRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return pypiRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

}
