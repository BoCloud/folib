package com.folib.repository;

import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPathResolver;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import javax.inject.Inject;
import java.util.LinkedHashSet;
import java.util.Set;

@Slf4j
@Component
public class CjpmRepositoryFeatures
        implements RepositoryFeatures {

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
