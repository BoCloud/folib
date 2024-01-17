package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.providers.header.HeaderMappingRegistry;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.repository.DockerRepositoryFeatures;
import com.veadan.folib.repository.DockerRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
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


    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    @Inject
    private DockerRepositoryManagementStrategy dockerRepositoryManagementStrategy;

    @Inject
    private DockerRepositoryFeatures dockerRepositoryFeatures;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @PostConstruct
    public void register() {
        headerMappingRegistry.register(ALIAS, USER_AGENT_PREFIX);

        logger.info("Registered layout provider '" + getClass().getCanonicalName() + "' with alias '" + ALIAS + "'.");
    }

    protected DockerArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {
        logger.info("DockerArtifactCoordinates parse path [{}]", path);
        return DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

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

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

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
}
