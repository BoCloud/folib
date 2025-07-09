package com.folib.services.impl;

import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.repositories.ArtifactCoordinatesRepository;
import com.folib.services.ArtifactCoordinatesService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import jakarta.inject.Inject;
import java.util.Optional;

@Slf4j
@Service
public class ArtifactCoordinatesServiceImpl implements ArtifactCoordinatesService {
    @Lazy
    @Inject
    private ArtifactCoordinatesRepository artifactCoordinatesRepository;

    public GenericCoordinates findById(String uuid) {
        Optional<GenericCoordinates> artifactCoordinates = artifactCoordinatesRepository.findById(uuid);
        return artifactCoordinates.orElse(null);
    }
}
