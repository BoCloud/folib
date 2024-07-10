package com.veadan.folib.providers.layout;

import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.DockerAuthComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.enums.DockerHeaderEnum;
import com.veadan.folib.providers.header.HeaderMappingRegistry;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.repository.DockerRepositoryFeatures;
import com.veadan.folib.repository.DockerRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Veadan
 */
@Component
public class DockerLayoutProvider
        extends AbstractLayoutProvider<DockerArtifactCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(DockerLayoutProvider.class);

    public static final String ALIAS = DockerArtifactCoordinates.LAYOUT_NAME;

    public static final String IMAGES_MANIFEST = "manifest.json";

    public static final String USER_AGENT_PREFIX = ALIAS;

    public static final String BLOBS = "blobs";

    public static final String MANIFEST = "manifest";

    public static final String MANIFESTS = "manifests";

    public static final List<String> DOCKER_BLOBS_MANIFESTS_URL = Lists.newArrayList(MANIFESTS, BLOBS);

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    @Inject
    private DockerRepositoryManagementStrategy dockerRepositoryManagementStrategy;

    @Inject
    private DockerRepositoryFeatures dockerRepositoryFeatures;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private DockerAuthComponent dockerAuthComponent;

    @PostConstruct
    public void register() {
        headerMappingRegistry.register(ALIAS, USER_AGENT_PREFIX);

        logger.info("Registered layout provider '" + getClass().getCanonicalName() + "' with alias '" + ALIAS + "'.");
    }

    @Override
    public DockerArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {
        logger.debug("DockerArtifactCoordinates parse path [{}]", path);
        return DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        // TODO: Fix
        return path.getFileName().toString().endsWith(".sha256");
    }

    public boolean isMetadata(RepositoryPath path) {
        // TODO: Fix
        return Objects.equals(IMAGES_MANIFEST, path.getFileName().toString());
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && !isMetadata(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case METADATA:
                    value = (Boolean) value || isMetadata(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case EXPIRED:
                    final Instant tenSecondsAgo = Instant.now().minus(10, ChronoUnit.SECONDS);
                    value = BooleanUtils.isTrue((Boolean) value) || ((DockerArtifactCoordinates.isDockerTag(repositoryPath) || DockerArtifactCoordinates.isRealManifestPath(repositoryPath))
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    tenSecondsAgo));

                    result.put(attributeType, value);

                    break;
                case REFRESH_CONTENT:
                    final Instant thirtySecondsAgo = Instant.now().minus(30, ChronoUnit.SECONDS);
                    value = BooleanUtils.isTrue((Boolean) value) || (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && (DockerArtifactCoordinates.isDockerTag(repositoryPath) || DockerArtifactCoordinates.isRealManifestPath(repositoryPath))
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    thirtySecondsAgo));

                    result.put(attributeType, value);

                    break;
                default:

                    break;
            }
        }

        return result;
    }

    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return dockerRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return dockerRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.SHA_256).collect(Collectors.toSet());
    }

    @Override
    public void initData(String storageId, String repositoryId) {
        try {
            for (String key : GlobalConstants.DOCKER_LAYER_DIR_NAME_LIST) {
                ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, key);
                artifactIdGroupRepository.saveOrUpdate(artifactIdGroup);
            }
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public void targetUrl(RepositoryPath path) {
        String remoteUrl = path.getRepository().getRemoteRepository().getUrl();
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        if (remoteUrl.endsWith(GlobalConstants.DOCKER_V2)) {
            try {
                String artifactPath = RepositoryFiles.relativizePath(path), imagePath = "";
                boolean concat = false;
                DockerArtifactCoordinates dockerArtifactCoordinates;
                if (DockerLayoutProvider.DOCKER_BLOBS_MANIFESTS_URL.stream().noneMatch(artifactPath::contains)) {
                    dockerArtifactCoordinates = DockerArtifactCoordinates.parse(artifactPath);
                    imagePath = dockerArtifactCoordinates.getName();
                    if (imagePath.split(GlobalConstants.SEPARATOR).length <= 1) {
                        concat = true;
                    }
                } else {
                    dockerArtifactCoordinates = DockerArtifactCoordinates.parse(path.getTargetUrl());
                    imagePath = dockerArtifactCoordinates.getName();
                    if (imagePath.split(GlobalConstants.SEPARATOR).length <= 2) {
                        concat = true;
                    }
                }
                if (concat) {
                    remoteUrl = remoteUrl.concat(GlobalConstants.SEPARATOR).concat(GlobalConstants.DOCKER_DEFAULT_REPO);
                }
            } catch (Exception ex) {
                logger.warn("RepositoryPath [{}] parse coordinates error [{}]", path, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }
        path.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(path.getTargetUrl(), GlobalConstants.SEPARATOR)));
        authToken(path);
        logger.debug("path [{}] docker headers [{}]", path, path.getHeaders());
    }

    private void authToken(RepositoryPath path) {
        try {
            RemoteRepository remoteRepository = path.getRepository().getRemoteRepository();
            String storageId = path.getStorageId(), repositoryId = path.getRepositoryId();
            DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
            path.setHeaders(DockerHeaderEnum.acceptHeaders());
            String imagePath = dockerArtifactCoordinates.getName();
            if (StringUtils.isNotBlank(path.getArtifactPath())) {
                imagePath = path.getArtifactPath();
            }
            dockerAuthComponent.handleAuthToken(remoteRepository, storageId, repositoryId, imagePath, path.getHeaders());
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
        }
    }
}
