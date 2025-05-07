package com.veadan.folib.services.impl;

import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.repositories.ArtifactCoordinatesRepository;
import com.veadan.folib.services.ArtifactCoordinatesService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.util.Optional;

@Slf4j
@Service
public class ArtifactCoordinatesServiceImpl implements ArtifactCoordinatesService {
    @Lazy
    @Inject
    private ArtifactCoordinatesRepository artifactCoordinatesRepository;

    public GenericArtifactCoordinates findById(String uuid) {
        Optional<GenericArtifactCoordinates> artifactCoordinates = artifactCoordinatesRepository.findById(uuid);
        return artifactCoordinates.orElse(null);
    }
}
