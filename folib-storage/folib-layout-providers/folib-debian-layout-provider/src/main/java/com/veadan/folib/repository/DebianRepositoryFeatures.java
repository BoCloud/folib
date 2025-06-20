package com.veadan.folib.repository;

import com.veadan.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.veadan.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.veadan.folib.storage.validation.deployment.RedeploymentValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

@Slf4j
@Component
public class DebianRepositoryFeatures implements RepositoryFeatures {

//    @Inject
//    private RedeploymentValidator redeploymentValidator;

//    @Inject
//    private GenericReleaseVersionValidator genericReleaseVersionValidator;
//
//    @Inject
//    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;

    private Set<String> defaultArtifactCoordinateValidators;

    @PostConstruct
    public void init() {
        defaultArtifactCoordinateValidators = new HashSet<>();
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }


}
