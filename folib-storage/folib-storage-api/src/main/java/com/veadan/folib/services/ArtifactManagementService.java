package com.veadan.folib.services;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.io.LayoutInputStream;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RepositoryStreamSupport;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.storage.validation.deployment.RedeploymentValidator;
import org.apache.commons.io.IOUtils;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.io.LayoutOutputStream;
import com.veadan.folib.io.StreamUtils;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.checksum.ArtifactChecksum;
import com.veadan.folib.storage.checksum.ChecksumCacheManager;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.ArtifactCoordinatesValidator;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.veadan.folib.storage.validation.artifact.version.VersionValidationException;
import com.veadan.folib.storage.validation.resource.ArtifactOperationsValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.FileSystemUtils;

/**
 * @author mtodorov
 */
@Component
public class ArtifactManagementService
{
    private static final Logger logger = LoggerFactory.getLogger(ArtifactManagementService.class);

    @Inject
    protected ArtifactOperationsValidator artifactOperationsValidator;

    @Inject
    protected ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected ArtifactRepository artifactEntityRepository;

    @Inject
    protected LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected ChecksumCacheManager checksumCacheManager;

    @Inject
    protected ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    public long validateAndStore(RepositoryPath repositoryPath,
                                 InputStream is)
            throws IOException,
            ProviderImplementationException,
            ArtifactCoordinatesValidationException
    {
        performRepositoryAcceptanceValidation(repositoryPath);
        return doStore(repositoryPath, is);
    }

    public long validateAndStore(RepositoryPath repositoryPath,
                                 Path sourcePath)
            throws IOException,
            ProviderImplementationException,
            ArtifactCoordinatesValidationException
    {
        performRepositoryAcceptanceValidation(repositoryPath);
        return doStore(repositoryPath, sourcePath);
    }

    public void validateAndStoreIndex(RepositoryPath repositoryPath)
            throws IOException,
            ProviderImplementationException,
            ArtifactCoordinatesValidationException
    {
        performStoreIndexRepositoryAcceptanceValidation(repositoryPath);
        doStoreIndex(repositoryPath);
    }

    public long store(RepositoryPath repositoryPath,
                      InputStream is)
            throws IOException
    {
        return doStore(repositoryPath, is);
    }

    public long store(RepositoryPath repositoryPath,
                      RepositoryPath sourcePath)
            throws IOException
    {
        return doStore(repositoryPath, sourcePath);
    }

    private long doStore(RepositoryPath repositoryPath,
                         InputStream is)
            throws IOException
    {
        long  startTime = System.currentTimeMillis();
        long result;
        try (final RepositoryStreamSupport.RepositoryOutputStream aos = artifactResolutionService.getOutputStream(repositoryPath))
        {
            result = writeArtifact(repositoryPath, is, aos);
            logger.info("Stored [{}] bytes for [{}].", result, repositoryPath);
            aos.flush();
        }
        catch (IOException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new ArtifactStorageException(e);
        }
        logger.info("DoStore [{}] take time [{}] ms." , repositoryPath.toString(), System.currentTimeMillis() - startTime);

        return result;
    }

    private long doStore(RepositoryPath repositoryPath,
                         Path sourcePath)
            throws IOException
    {
        long  startTime = System.currentTimeMillis();
        long result;
        try (final RepositoryStreamSupport.RepositoryOutputStream aos = artifactResolutionService.getOutputStream(repositoryPath))
        {
            result = writeArtifact(repositoryPath, sourcePath, aos);
            logger.debug("Stored [{}] bytes for [{}].", result, repositoryPath);
            aos.flush();
        }
        catch (IOException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new ArtifactStorageException(e);
        }
        logger.debug("DoStore [{}] take time [{}] ms" , repositoryPath.toString(), System.currentTimeMillis() - startTime);

        return result;
    }

    private void doStoreIndex(RepositoryPath repositoryPath)
            throws IOException
    {
        try (final RepositoryStreamSupport.RepositoryStoreIndexInputStream ins = artifactResolutionService.getStoreIndexInputStream(repositoryPath))
        {
            writeArtifactIndex(repositoryPath, ins);
            logger.debug("Stored index for [{}].", repositoryPath);
            ins.commitStoreIndex();
        }
        catch (IOException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new ArtifactStorageException(e);
        }
    }

    private long writeArtifact(RepositoryPath repositoryPath,
                               InputStream is,
                               OutputStream os)
            throws IOException
    {
        LayoutOutputStream aos = StreamUtils.findSource(LayoutOutputStream.class, os);

        Repository repository = repositoryPath.getRepository();

        Boolean checksumAttribute = RepositoryFiles.isChecksum(repositoryPath);

        // If we have no digests, then we have a checksum to store.
        if (Boolean.TRUE.equals(checksumAttribute))
        {
            aos.setCacheOutputStream(new ByteArrayOutputStream());
        }

        if (repository.isHostedRepository())
        {
            artifactEventListenerRegistry.dispatchArtifactUploadingEvent(repositoryPath);
        }

        long startTime = System.currentTimeMillis();
        long totalAmountOfBytes = IOUtils.copy(is, os);
        logger.info("IOUtils copy [{}] take time [{}] ms" , repositoryPath.toString(), System.currentTimeMillis() - startTime);

        URI repositoryPathId = repositoryPath.toUri();
        Map<String, String> digestMap = aos.getDigestMap(repository.getLayout());
        if (Boolean.FALSE.equals(checksumAttribute) && !digestMap.isEmpty())
        {
            // Store artifact digests in cache if we have them.
            addChecksumsToCacheManager(digestMap, repositoryPathId);

            writeChecksums(repositoryPath, digestMap);
        }

        if (Boolean.TRUE.equals(checksumAttribute))
        {
            byte[] checksumValue = ((ByteArrayOutputStream) aos.getCacheOutputStream()).toByteArray();
            if (checksumValue != null && checksumValue.length > 0)
            {
                // Validate checksum with artifact digest cache.
                validateUploadedChecksumAgainstCache(checksumValue, repositoryPathId);
            }
        }

        return totalAmountOfBytes;
    }

    private long writeArtifact(RepositoryPath repositoryPath,
                               Path sourcePath,
                               OutputStream os)
            throws IOException
    {
        LayoutOutputStream aos = StreamUtils.findSource(LayoutOutputStream.class, os);

        Repository repository = repositoryPath.getRepository();

        Boolean checksumAttribute = RepositoryFiles.isChecksum(repositoryPath);

        // If we have no digests, then we have a checksum to store.
        if (Boolean.TRUE.equals(checksumAttribute))
        {
            aos.setCacheOutputStream(new ByteArrayOutputStream());
        }

        if (repository.isHostedRepository())
        {
            artifactEventListenerRegistry.dispatchArtifactUploadingEvent(repositoryPath);
        }

        long startTime = System.currentTimeMillis();
        long totalAmountOfBytes = Files.copy(sourcePath, os);
        logger.debug("Files copy [{}] take time [{}] ms" , repositoryPath.toString(), System.currentTimeMillis() - startTime);

        URI repositoryPathId = repositoryPath.toUri();
        Map<String, String> digestMap = aos.getDigestMap(repository.getLayout());
        if (Boolean.FALSE.equals(checksumAttribute) && !digestMap.isEmpty())
        {
            // Store artifact digests in cache if we have them.
            addChecksumsToCacheManager(digestMap, repositoryPathId);

            writeChecksums(repositoryPath, digestMap);
        }

        if (Boolean.TRUE.equals(checksumAttribute))
        {
            byte[] checksumValue = ((ByteArrayOutputStream) aos.getCacheOutputStream()).toByteArray();
            if (checksumValue != null && checksumValue.length > 0)
            {
                // Validate checksum with artifact digest cache.
                validateUploadedChecksumAgainstCache(checksumValue, repositoryPathId);
            }
        }

        return totalAmountOfBytes;
    }

    private void writeArtifactIndex(RepositoryPath repositoryPath,
                                    InputStream is)
            throws IOException
    {
        LayoutInputStream ins = StreamUtils.findSource(LayoutInputStream.class, is);
        if (Objects.isNull(ins)) {
            throw new IOException("repositoryPath LayoutInputStream not exists");
        }
        byte [] bytes = new byte[8192];
        while (ins.read(bytes) != -1) {

        }
        Repository repository = repositoryPath.getRepository();

        Boolean checksumAttribute = RepositoryFiles.isChecksum(repositoryPath);

        if (repository.isHostedRepository())
        {
            artifactEventListenerRegistry.dispatchArtifactUploadingEvent(repositoryPath);
        }

        URI repositoryPathId = repositoryPath.toUri();
        Set<String> digestAlgorithmSet = repositoryPath.getFileSystem().getDigestAlgorithmSet();
        digestAlgorithmSet.forEach(item -> ins.getMessageDigestAsHexadecimalString(item, repository.getLayout()));
        Map<String, String> digestMap = ins.getDigestMap();
        if (Boolean.FALSE.equals(checksumAttribute) && !digestMap.isEmpty())
        {
            // Store artifact digests in cache if we have them.
            addChecksumsToCacheManager(digestMap, repositoryPathId);
            writeChecksums(repositoryPath, digestMap);
        }
    }

    private void writeChecksums(RepositoryPath repositoryPath,
                                Map<String, String> digestMap)
    {
        LayoutFileSystemProvider provider = (LayoutFileSystemProvider) repositoryPath.getFileSystem().provider();

        digestMap.entrySet()
                .stream()
                .forEach(entry -> {
                    final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, entry.getKey());
                    try
                    {
                        Files.write(checksumPath, entry.getValue().getBytes(StandardCharsets.UTF_8));
                    }
                    catch (IOException ex)
                    {
                        logger.error(ex.getMessage(), ex);
                    }
                });
    }

    public void checksums(RepositoryPath repositoryPath, Map<String, String> digestMap)
    {
        if (Objects.nonNull(digestMap) && !digestMap.isEmpty())
        {
            addChecksumsToCacheManager(digestMap, repositoryPath.toUri());
            writeChecksums(repositoryPath, digestMap);
        }
    }

    private void validateUploadedChecksumAgainstCache(byte[] checksum,
                                                      URI artifactPathId)
    {
        logger.debug("Received checksum: {}", new String(checksum, StandardCharsets.UTF_8));

        String artifactPath = artifactPathId.toString();
        String artifactBasePath = artifactPath.substring(0, artifactPath.lastIndexOf('.'));
        String checksumExtension = artifactPath.substring(artifactPath.lastIndexOf('.') + 1, artifactPath.length());

        if (!matchesChecksum(checksum, artifactBasePath, checksumExtension))
        {
            logger.error("The checksum for {} [{}] is invalid!",
                    artifactPath,
                    new String(checksum, StandardCharsets.UTF_8));
        }

        checksumCacheManager.removeArtifactChecksum(artifactBasePath, checksumExtension);
    }

    private boolean matchesChecksum(byte[] pChecksum,
                                    String artifactBasePath,
                                    String checksumExtension)
    {
        String checksum = new String(pChecksum, StandardCharsets.UTF_8);
        ArtifactChecksum artifactChecksum = checksumCacheManager.getArtifactChecksum(artifactBasePath);

        if (artifactChecksum == null)
        {
            return false;
        }

        Map<Boolean, Set<String>> matchingMap = artifactChecksum.getChecksums()
                .entrySet()
                .stream()
                .collect(Collectors.groupingBy(e -> e.getValue()
                                .equals(checksum),
                        Collectors.mapping(
                                e -> e.getKey(),
                                Collectors.toSet())));

        Set<String> matched = matchingMap.get(Boolean.TRUE);
        Set<String> unmatched = matchingMap.get(Boolean.FALSE);

        logger.debug("Artifact checksum matchings: artifact-[{}]; ext-[{}]; matched-[{}];" +
                        " unmatched-[{}]; checksum-[{}]",
                artifactBasePath,
                checksumExtension,
                matched,
                unmatched,
                checksum);

        return matched != null && !matched.isEmpty();
    }

    private void addChecksumsToCacheManager(Map<String, String> digestMap,
                                            URI artifactPath)
    {
        digestMap.entrySet()
                .stream()
                .forEach(e -> checksumCacheManager.addArtifactChecksum(artifactPath.toString(), e.getKey(), e.getValue()));
    }

    public boolean performRepositoryAcceptanceValidation(RepositoryPath path)
            throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException
    {
        logger.debug("Validate artifact with path [{}]", path);

        Repository repository = path.getFileSystem().getRepository();

        artifactOperationsValidator.validate(path);

        if (!RepositoryFiles.isArtifact(path))
        {
            return true;
        }

        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(path);
        logger.debug("Validate artifact with coordinates [{}]", coordinates);

        try
        {
            for (String validatorKey : repository.getArtifactCoordinateValidators())
            {
                ArtifactCoordinatesValidator validator = artifactCoordinatesValidatorRegistry.getProvider(
                        validatorKey);
                if (validator.supports(repository))
                {
                    validator.validate(repository, coordinates);
                }
            }
        }
        catch (VersionValidationException e)
        {
            throw new ArtifactStorageException(e);
        }

        artifactOperationsValidator.checkAllowsRedeployment(repository, coordinates);
        artifactOperationsValidator.checkAllowsDeployment(repository);

        return true;
    }

    private boolean performStoreIndexRepositoryAcceptanceValidation(RepositoryPath path)
            throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException
    {
        logger.debug("Validate artifact with path [{}]", path);

        Repository repository = path.getFileSystem().getRepository();

        artifactOperationsValidator.validate(path);

        if (!RepositoryFiles.isArtifact(path))
        {
            return true;
        }

        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(path);
        logger.debug("Validate artifact with coordinates [{}]", coordinates);

        try
        {
            for (String validatorKey : repository.getArtifactCoordinateValidators())
            {
                if (RedeploymentValidator.ALIAS.equals(validatorKey)) {
                    continue;
                }
                ArtifactCoordinatesValidator validator = artifactCoordinatesValidatorRegistry.getProvider(
                        validatorKey);
                if (validator.supports(repository))
                {
                    validator.validate(repository, coordinates);
                }
            }
        }
        catch (VersionValidationException e)
        {
            throw new ArtifactStorageException(e);
        }
        return true;
    }

    protected Storage getStorage(String storageId)
    {
        return getConfiguration().getStorages().get(storageId);
    }

    protected Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

    @Transactional(rollbackFor = Exception.class)
    public void delete(RepositoryPath repositoryPath,
                       boolean force)
            throws IOException
    {
        artifactOperationsValidator.validate(repositoryPath);

        final Repository repository = repositoryPath.getRepository();

        artifactOperationsValidator.checkAllowsDeletion(repository);

        Optional<Artifact> artifactEntry = Optional.ofNullable(repositoryPath.getArtifactEntry());
        if (!Files.isDirectory(repositoryPath) && RepositoryFiles.isArtifact(repositoryPath) && artifactEntry.isEmpty())
        {
            logger.warn(String.format("Corresponding [%s] record not found for path [%s]",
                    Artifact.class.getSimpleName(), repositoryPath));
        }

        try
        {
            RepositoryFiles.delete(repositoryPath, force);
        }
        catch (IOException e)
        {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    public void copy(RepositoryPath srcPath, RepositoryPath destPath)
            throws IOException
    {
        artifactOperationsValidator.validate(srcPath);

        if (Files.isDirectory(srcPath))
        {
            FileSystemUtils.copyRecursively(srcPath, destPath);
        }
        else
        {
            Files.copy(srcPath, destPath);
        }
    }

}
