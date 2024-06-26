package com.veadan.folib.providers.layout;


import com.veadan.folib.artifact.coordinates.HuggingFaceArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.storage.repository.HuggingFaceRepositoryFeatures;
import com.veadan.folib.storage.repository.HuggingFaceRepositoryManagementStrategy;
import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;

import static com.veadan.folib.constant.MlModelConstants.LATEST_LEAD_FILE_NAME;
import static com.veadan.folib.constant.MlModelConstants.LEAD_FILE_NAME;

@Component("huggingFaceLayoutProvider")
public class HuggingFaceLayoutProvider extends AbstractLayoutProvider<HuggingFaceArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(HuggingFaceLayoutProvider.class);

    public  static final String ALIAS = HuggingFaceArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private HuggingFaceRepositoryManagementStrategy huggingFaceRepositoryManagementStrategy;
    @Inject
    private HuggingFaceRepositoryFeatures huggingFaceRepositoryFeatures;

    @PostConstruct
    public void register() {
        logger.info("Registered Layout provider '{}' with alias '{}.'",
        getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public HuggingFaceArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return new HuggingFaceArtifactCoordinates(RepositoryFiles.relativizePath(repositoryPath));
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public HuggingFaceRepositoryManagementStrategy getRepositoryManagementStrategy() {
        return huggingFaceRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return huggingFaceRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
            Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                    attributeTypes);

            for (RepositoryFileAttributeType attributeType : attributeTypes) {
                Object value = result.get(attributeType);
                switch (attributeType) {
                    case ARTIFACT:
                        value = (Boolean) value && !isHfmlArtifact(repositoryPath);

                        if (value != null) {
                            result.put(attributeType, value);
                        }

                        break;
                    //case METADATA:
                    //    value = (Boolean) value || isPubMetadata(repositoryPath);
                    //
                    //    if (value != null) {
                    //        result.put(attributeType, value);
                    //    }
                    //
                    //    break;
                    case REFRESH_CONTENT:
                        final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                        value = BooleanUtils.isTrue((Boolean) value) || (Files.exists(repositoryPath) && isHfmlPackageJson(repositoryPath)
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

    public boolean isHfmlArtifact(RepositoryPath path) {
        return path.getFileName().toString().lastIndexOf('.')>-1;
    }

    public boolean isHfmlPackageJson(RepositoryPath path) {
        return (LEAD_FILE_NAME.equals(path.getFileName().toString()) || LATEST_LEAD_FILE_NAME.equals(path.getFileName().toString()));
    }
}
