package com.folib.repository;

import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

@Component
public class PubRepositoryFeatures implements RepositoryFeatures {

    private static final int CHANGES_BATCH_SIZE = 500;

    private static final boolean ALLOWS_UN_PUBLISH_DEFAULT = true;

    private static final Logger logger = LoggerFactory.getLogger(PubRepositoryFeatures.class);

    @Lazy
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Lazy
    @Inject
    private RedeploymentValidator redeploymentValidator;
    @Lazy
    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;
    @Lazy
    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;
    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    private Set<String> defaultArtifactCoordinateValidators;

    @PostConstruct
    public void init() {
        defaultArtifactCoordinateValidators = new LinkedHashSet<>(Arrays.asList(redeploymentValidator.getAlias(),
                genericReleaseVersionValidator.getAlias(),
                genericSnapshotVersionValidator.getAlias()));
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }

    protected Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
