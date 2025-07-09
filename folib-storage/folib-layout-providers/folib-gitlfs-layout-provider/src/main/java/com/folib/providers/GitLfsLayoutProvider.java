package com.folib.providers;


import com.folib.artifact.coordinates.GitLfsCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.storage.repository.GitLfsRepositoryFeatures;
import com.folib.storage.repository.GitLfsRepositoryStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;


@Component("gitLfsLayoutProvider")
public class GitLfsLayoutProvider
        extends AbstractLayoutProvider<GitLfsCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(GitLfsLayoutProvider.class);

    public static final String ALIAS = GitLfsCoordinates.LAYOUT_NAME;

    @Inject
    private GitLfsRepositoryStrategy gitLfsRepositoryManagementStrategy;

    @Inject
    private GitLfsRepositoryFeatures gitLfsRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public GitLfsCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return new GitLfsCoordinates(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return false;
    }


    @Override
    public GitLfsRepositoryStrategy getRepositoryManagementStrategy()
    {
        return gitLfsRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return gitLfsRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

}
