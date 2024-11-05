package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.HelmArtifactCoordinates;
import com.veadan.folib.providers.header.HeaderMappingRegistry;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.HelmRepositoryFeatures;
import com.veadan.folib.repository.HelmRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

@Component
public class HelmLayoutProvider  extends AbstractLayoutProvider<HelmArtifactCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(HelmLayoutProvider.class);

    @Inject
    private HelmRepositoryManagementStrategy helmRepositoryManagementStrategy;

    @Inject
    private HelmRepositoryFeatures helmRepositoryFeatures;

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    public static final String ALIAS ="helm";

    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return helmRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return helmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return "helm";
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public HelmArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return HelmArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath),repositoryPath.getFileName().toString());
    }

    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
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
                    value = (Boolean) value && !isHelmMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isHelmMetadata(repositoryPath);

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

    public boolean isHelmMetadata(RepositoryPath repositoryPath) {
        return !repositoryPath.getFileName().toString().endsWith(".tgz");
    }

}
