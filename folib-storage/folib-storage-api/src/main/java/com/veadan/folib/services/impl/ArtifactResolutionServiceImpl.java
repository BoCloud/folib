package com.veadan.folib.services.impl;

import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RepositoryStreamSupport;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.resource.ArtifactOperationsValidator;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

/**
 * @author mtodorov
 */
@Component
public class ArtifactResolutionServiceImpl
        implements ArtifactResolutionService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ArtifactOperationsValidator artifactOperationsValidator;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Override
    public RepositoryStreamSupport.RepositoryInputStream getInputStream(RepositoryPath path)
            throws IOException {
        Repository repository = path.getFileSystem().getRepository();
//        artifactOperationsValidator.validate(path);

        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        return (RepositoryStreamSupport.RepositoryInputStream) repositoryProvider.getInputStream(path);
    }

    @Override
    public RepositoryStreamSupport.RepositoryStoreIndexInputStream getStoreIndexInputStream(RepositoryPath path) throws IOException {
        Repository repository = path.getFileSystem().getRepository();
        artifactOperationsValidator.validate(path);

        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        return (RepositoryStreamSupport.RepositoryStoreIndexInputStream) repositoryProvider.getStoreIndexInputStream(path);
    }

    @Override
    public RepositoryStreamSupport.RepositoryOutputStream getOutputStream(RepositoryPath repositoryPath)
            throws IOException,
            NoSuchAlgorithmException {
        artifactOperationsValidator.validate(repositoryPath);

        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());

        RepositoryStreamSupport.RepositoryOutputStream os = (RepositoryStreamSupport.RepositoryOutputStream) repositoryProvider.getOutputStream(repositoryPath);
        if (os == null) {
            throw new ArtifactStorageException("Artifact " + repositoryPath + " cannot be stored.");
        }
        return os;
    }

    public Storage getStorage(String storageId) {
        return configurationManager.getConfiguration().getStorage(storageId);
    }

    @Override
    public RepositoryPath resolvePath(String storageId,
                                      String repositoryId,
                                      String artifactPath)
            throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) repositoryProvider.fetchPath(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            return null;
        }
    }

    @Override
    public RepositoryPath resolvePath(String storageId, String repositoryId, String targetUrl, String artifactPath) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        repositoryPath.setTargetUrl(targetUrl);
        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) repositoryProvider.fetchPath(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            return null;
        }
    }

    @Override
    public RepositoryPath resolvePath(RepositoryPath repositoryPath) throws IOException {
        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) repositoryProvider.fetchPath(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            return null;
        }
    }

}
