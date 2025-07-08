package com.folib.providers.layout;


import com.folib.artifact.coordinates.GitLfsArtifactCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.GitLfsRepositoryFeatures;
import com.folib.storage.repository.GitLfsRepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;


@Component("gitLfsLayoutProvider")
public class GitLfsLayoutProvider
        extends AbstractLayoutProvider<GitLfsArtifactCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(GitLfsLayoutProvider.class);

    public static final String ALIAS = GitLfsArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private GitLfsRepositoryManagementStrategy gitLfsRepositoryManagementStrategy;

    @Inject
    private GitLfsRepositoryFeatures gitLfsRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public GitLfsArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return new GitLfsArtifactCoordinates(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return false;
    }


    @Override
    public GitLfsRepositoryManagementStrategy getRepositoryManagementStrategy()
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
