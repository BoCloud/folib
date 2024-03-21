package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.ConanArtifactCoordinates;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.providers.header.HeaderMappingRegistry;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.ConanRepositoryFeatures;
import com.veadan.folib.repository.ConanRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;

@Component
public class ConanLayoutProvider extends AbstractLayoutProvider<ConanArtifactCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(ConanLayoutProvider.class);

    @Inject
    private ConanRepositoryManagementStrategy conanRepositoryManagementStrategy;

    @Inject
    private ConanRepositoryFeatures conanRepositoryFeatures;

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    public static final String ALIAS = "conan";

    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return conanRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return conanRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return "conan";
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public ConanArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return ConanArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
    }

    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public void targetUrl(RepositoryPath path) {
        if (StringUtils.isNotBlank(path.getTargetUrl())) {
            String remoteUrl = path.getRepository().getRemoteRepository().getUrl();
            remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
            path.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(path.getTargetUrl(), GlobalConstants.SEPARATOR)));
        }
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
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(30, ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (repositoryPath.getFileName().toString().endsWith("index.json")
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    halfAnHourAgo));
                    result.put(attributeType, value);
                    break;
                default:

                    break;
            }
        }
        return result;
    }

}

