package com.veadan.folib.providers.repository;

import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.*;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * @author Veadan
 */
@Component
public class HostedRepositoryProvider extends AbstractRepositoryProvider {

    private static final Logger logger = LoggerFactory.getLogger(HostedRepositoryProvider.class);

    private static final String ALIAS = "hosted";

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath repositoryPath) throws IOException {
        try {
            return Files.newInputStream(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            logger.info("The path [{}] does not exist!\n*\t[{}]", repositoryPath, e.getMessage());

            return null;
        } catch (IOException ex) {
            logger.error("Failed to decorate InputStream for [{}]", repositoryPath, ex);

            throw ex;
        }
    }

    @Override
    public OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException {
        return Files.newOutputStream(repositoryPath);
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator) {
        List<Path> result = new LinkedList<Path>();

        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        long startTime = System.currentTimeMillis();
        List<Artifact> searchResult = artifactIdGroupRepository.findArtifactsGremlin(storageId, repositoryId, predicate.getArtifactId(),
                predicate.getUseArtifactName(), predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit());
        logger.info("FindArtifacts storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] skip [{}] limit [{}] useLimit [{}] artifactListSize [{}] take time [{}] ms", storageId, repositoryId, predicate.getArtifactId(), predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit(), searchResult.size(), System.currentTimeMillis() - startTime);
        for (Artifact artifactEntry : searchResult) {

            try {
                result.add(rootRepositoryPath.resolve(artifactEntry));
            } catch (Exception e) {
                logger.error("Failed to resolve Artifact [{}]",
                        artifactEntry.getArtifactCoordinates(), e);
                continue;
            }
        }
        return result;
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonCountArtifacts(storageId, repositoryId, predicate.getArtifactId(),
                predicate.getUseArtifactName(), predicate.getCoordinateValues());
    }

    @Override
    protected RepositoryPath fetchPath(RepositoryPath repositoryPath)
            throws IOException {
        logger.debug(" -> Checking local cache for {} ...", repositoryPath);
        if (artifactNotExists(repositoryPath)) {
            logger.info("The artifact {} was not found in the local cache", repositoryPath);
            return null;
        }
        logger.debug("The artifact {} was found in the local cache", repositoryPath);
        return repositoryPath;
    }

    private boolean artifactNotExists(RepositoryPath repositoryPath) throws IOException {
        return !RepositoryFiles.artifactExists(repositoryPath);
    }

}
