package com.veadan.folib.providers.io;

import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;

/**
 * @author veadan
 * @date 1/9/2024 16:51
 */
@Component
public class GoMetadataExpiredRepositoryPathHandler implements GoExpiredRepositoryPathHandler{
    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;
    @Override
    public boolean supports(final RepositoryPath repositoryPath)
            throws IOException
    {
        if (repositoryPath == null)
        {
            return false;
        }

        if (!RepositoryFiles.isMetadata(repositoryPath))
        {
            return false;
        }

        Repository repository = repositoryPath.getRepository();
        return ((RepositoryData)repository).getRemoteRepository() != null;
    }
    @Override
    public void handleExpiration(RepositoryPath repositoryPath) throws IOException {
        proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
    }
}
