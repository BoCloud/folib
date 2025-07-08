package com.folib.storage.repository;


import com.folib.repository.RepositoryFeatures;
import org.springframework.stereotype.Component;


import java.util.LinkedHashSet;
import java.util.Set;

@Component
public class CargoRepositoryFeatures implements RepositoryFeatures {

    private final Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }
}
