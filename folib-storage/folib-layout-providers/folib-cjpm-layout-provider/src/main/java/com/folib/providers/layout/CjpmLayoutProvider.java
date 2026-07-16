package com.folib.providers.layout;

import com.folib.artifact.coordinates.CjpmCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.CjpmRepositoryFeatures;
import com.folib.repository.CjpmRepositoryStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import jakarta.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;

@Component("cjpmLayoutProvider")
public class CjpmLayoutProvider
        extends AbstractLayoutProvider<CjpmCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(CjpmLayoutProvider.class);

    public static final String ALIAS = CjpmCoordinates.LAYOUT_NAME;

    @Inject
    private CjpmRepositoryStrategy cjpmRepositoryStrategy;

    @Inject
    private CjpmRepositoryFeatures cjpmRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public CjpmCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return new CjpmCoordinates(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return false;
    }


    @Override
    public CjpmRepositoryStrategy getRepositoryManagementStrategy()
    {
        return cjpmRepositoryStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return cjpmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

}
