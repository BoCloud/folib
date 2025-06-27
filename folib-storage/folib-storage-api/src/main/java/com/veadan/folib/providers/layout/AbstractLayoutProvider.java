package com.veadan.folib.providers.layout;

import cn.hutool.crypto.digest.SM3;
import com.google.common.collect.ImmutableSet;
import com.veadan.folib.artifact.archive.*;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactGroup;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CacheUtil;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.web.util.UriComponentsBuilder;

import jakarta.inject.Inject;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 */
public abstract class AbstractLayoutProvider<T extends LayoutArtifactCoordinatesEntity>
        implements LayoutProvider<T> {

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    private static final ArchiveListingFunction ARCHIVE_LISTING_FUNCTION = new CompositeArchiveListingFunction(
            ImmutableSet.of(
                    ZipArchiveListingFunction.INSTANCE,
                    TarGzArchiveListingFunction.INSTANCE,
                    TarArchiveListingFunction.INSTANCE,
                    Bzip2ArchiveListingFunction.INSTANCE,
                    JarArchiveListingFunction.INSTANCE
            )
    );
    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;
    @Lazy
    @Inject
    protected StorageProviderRegistry storageProviderRegistry;
    @Lazy
    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Override
    public abstract Set<String> getDefaultArtifactCoordinateValidators();

    protected abstract boolean isArtifactMetadata(RepositoryPath repositoryPath);

    public abstract T getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException;

    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, SM3.ALGORITHM_NAME)
                .collect(Collectors.toSet());
    }

    public boolean isChecksum(RepositoryPath repositoryPath) {
        return isChecksum(repositoryPath.getFileName().toString());
    }

    public boolean isChecksum(String fileName) {
        for (String e : getDigestAlgorithmSet()) {
            if (fileName.toString().endsWith("." + e.replaceAll("-", "").toLowerCase())) {
                return true;
            }
        }

        return false;
    }


    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        if (attributeTypes == null || attributeTypes.length == 0) {
            return Collections.emptyMap();
        }

        Map<RepositoryFileAttributeType, Object> result = new HashMap<>();
        for (RepositoryFileAttributeType repositoryFileAttributeType : attributeTypes) {
            Object value;
            switch (repositoryFileAttributeType) {
                default:
                    Map<RepositoryFileAttributeType, Object> attributesLocal;
                    value = null;

                    break;
                case CHECKSUM:
                    value = isChecksum(repositoryPath);

                    break;
                case TEMP:
                    value = repositoryPath.isAbsolute()
                            && repositoryPath.startsWith(repositoryPath.getFileSystem()
                            .getRootDirectory()
                            .resolve(LayoutFileSystem.TEMP));

                    break;
                case TRASH:
                    value = repositoryPath.isAbsolute()
                            && repositoryPath.startsWith(repositoryPath.getFileSystem()
                            .getRootDirectory()
                            .resolve(LayoutFileSystem.TRASH));

                    break;
                case METADATA:
                    value = isArtifactMetadata(repositoryPath);

                    break;
                case ARTIFACT:
                    attributesLocal = getRepositoryFileAttributes(repositoryPath,
                            RepositoryFileAttributeType.CHECKSUM);

                    boolean isChecksum = Boolean.TRUE.equals(attributesLocal.get(RepositoryFileAttributeType.CHECKSUM));
                    boolean isDirectory = Files.isDirectory(repositoryPath);

                    value = !isChecksum && !isDirectory && !RepositoryFiles.isArtifactMetadata(repositoryPath);

                    break;
                case COORDINATES:
                    attributesLocal = getRepositoryFileAttributes(repositoryPath,
                            RepositoryFileAttributeType.ARTIFACT);

                    boolean isArtifact = Boolean.TRUE.equals(attributesLocal.get(RepositoryFileAttributeType.ARTIFACT));
                    if (!isArtifact) {
                        value = null;
                        break;
                    }

                    T artifactCoordinates = getArtifactCoordinates(repositoryPath);
                    artifactCoordinates.setUuid(artifactCoordinates.buildPath());

                    value = artifactCoordinates;
                    break;
                case RESOURCE_URL:
                    value = resolveResource(repositoryPath);

                    break;
                case ARTIFACT_PATH:
                    value = RepositoryFiles.relativizePath(repositoryPath);

                    break;

                case STORAGE_ID:
                    value = repositoryPath.getRepository().getStorage().getId();

                    break;

                case REPOSITORY_ID:
                    value = repositoryPath.getRepository().getId();

                    break;
                case EXPIRED:
                    value = false;

                    break;
                case REFRESH_CONTENT:
                    value = false;

                    break;

            }
            if (value != null) {
                result.put(repositoryFileAttributeType, value);
            }
        }

        return result;
    }

    public URL resolveResource(RepositoryPath repositoryPath)
            throws IOException {
        CacheUtil<String, URI> cacheUtil = CacheUtil.getInstance();
        String key = "uri";
        URI baseUri = cacheUtil.get(key);
        if (Objects.isNull(baseUri)) {
            baseUri = configurationManager.getBaseUri();
            cacheUtil.put(key, baseUri);
        }
        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();
        URI artifactResource = RepositoryFiles.resolveResource(repositoryPath);

        return UriComponentsBuilder.fromUri(baseUri)
                .pathSegment("storages", storage.getId(), repository.getId(), "/")
                .build()
                .toUri()
                .resolve(artifactResource)
                .toURL();
    }

    @Override
    public Set<String> listArchiveFilenames(final RepositoryPath repositoryPath) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.listFilenames(repositoryPath);
            } catch (IOException e) {
                logger.warn("Unable to list filenames in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return Collections.emptySet();
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, String fileName) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.getContentByFileName(repositoryPath, fileName);
            } catch (IOException e) {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.getContentByFileName(repositoryPath, path, fileName);
            } catch (IOException e) {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.getContentByEqualsFileName(repositoryPath, path, fileName);
            } catch (IOException e) {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return null;
    }

    @Override
    public Set<ArtifactGroup> getArtifactGroups(RepositoryPath path)
            throws IOException {
        if (!RepositoryFiles.isArtifact(path)) {
            return Collections.emptySet();
        }

        Repository repository = path.getFileSystem().getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates c = RepositoryFiles.readCoordinates(path);
        Optional<ArtifactIdGroup> artifactIdGroup = artifactIdGroupRepository.findAllArtifactsInGroup(storage.getId(),
                repository.getId(),
                c.getId());

        return artifactIdGroup.map(ArtifactGroup.class::cast).map(Collections::singleton).orElse(Collections.emptySet());
    }

    @Override
    public int refreshContentInterval(RepositoryPath repositoryPath) {
        String key = repositoryPath.getRepository().getLayout().toUpperCase() + "_REFRESH_CONTENT_INTERVAL";
        String refreshContentInterval = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(refreshContentInterval)) {
            return GlobalConstants.DEFAULT_REFRESH_CONTENT_INTERVAL;
        }
        return Integer.parseInt(refreshContentInterval);
    }

}
