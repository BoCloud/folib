package com.veadan.folib.layout.providers;

import com.veadan.folib.artifact.coordinates.CargoArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.AbstractLayoutProvider;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.storage.repository.CargoRepositoryFeatures;
import com.veadan.folib.storage.repository.CargoRepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

@Component("cargoLayoutProvider")
public class CargoLayoutProvider extends AbstractLayoutProvider<CargoArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(CargoLayoutProvider.class);
    public  static final String ALIAS = CargoArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private CargoRepositoryManagementStrategy cargoRepositoryManagementStrategy;

    @Inject
    private CargoRepositoryFeatures cargoRepositoryFeatures;

    @PostConstruct
    public void register() {
        logger.info("Registered Layout provider '{}' with alias '{}.'",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return cargoRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public CargoArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return new CargoArtifactCoordinates(RepositoryFiles.relativizePath(repositoryPath));
    }

    @Override
    public CargoRepositoryManagementStrategy getRepositoryManagementStrategy() {
        return cargoRepositoryManagementStrategy;
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
       //TODO
        return super.getRepositoryFileAttributes(repositoryPath, attributeTypes);
    }


}
