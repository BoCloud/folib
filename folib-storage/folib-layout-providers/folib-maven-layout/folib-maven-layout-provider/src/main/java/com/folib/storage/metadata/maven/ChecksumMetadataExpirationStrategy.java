package com.folib.storage.metadata.maven;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.commons.encryption.EncryptionAlgorithmsEnum;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class ChecksumMetadataExpirationStrategy
        implements MetadataExpirationStrategy
{

    private static final Logger logger = LoggerFactory.getLogger(ChecksumMetadataExpirationStrategy.class);

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    public Decision decide(final RepositoryPath repositoryPath) throws IOException
    {
        Decision decision = decideUsingChecksumAlgorithm(repositoryPath, EncryptionAlgorithmsEnum.SHA1);
        if (Decision.UNDECIDED.equals(decision))
        {
            decision = decideUsingChecksumAlgorithm(repositoryPath, EncryptionAlgorithmsEnum.MD5);
        }
        return decision;
    }

    private RepositoryPath resolveSiblingChecksum(final RepositoryPath repositoryPath,
                                                  final EncryptionAlgorithmsEnum checksumAlgorithm)
    {
        return repositoryPath.resolveSibling(
                repositoryPath.getFileName().toString() + checksumAlgorithm.getExtension());
    }

    private String readChecksum(final RepositoryPath checksumRepositoryPath)
            throws IOException
    {
        if (!Files.exists(checksumRepositoryPath))
        {
            return null;
        }

        return Files.readAllLines(checksumRepositoryPath).stream().findFirst().orElse(null);
    }

    private Decision decideUsingChecksumAlgorithm(final RepositoryPath repositoryPath,
                                                  final EncryptionAlgorithmsEnum checksumAlgorithm) throws IOException
    {
        RepositoryPath checksumRepositoryPath = resolveSiblingChecksum(repositoryPath, checksumAlgorithm);
        String currentChecksum = readChecksum(checksumRepositoryPath);
        if (currentChecksum == null)
        {
            logger.info("Unable to read local {} checksum for {}, returning " + Decision.UNDECIDED.name(),
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.UNDECIDED;
        }

        proxyRepositoryArtifactResolver.fetchRemoteResource(checksumRepositoryPath);
        final String newRemoteChecksum = readChecksum(checksumRepositoryPath);

        if (newRemoteChecksum == null)
        {
            logger.info("Unable to fetch remote {} checksum for {}, returning " + Decision.UNDECIDED.name(),
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.UNDECIDED;
        }

        if (currentChecksum.equals(newRemoteChecksum))
        {
            logger.info("Local and remote {} checksums match for {}, no need to refetch metadata",
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.USABLE;
        }
        else
        {
            logger.info("Local and remote {} checksums differ for {}, will refetch metadata",
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.EXPIRED;
        }
    }

}
