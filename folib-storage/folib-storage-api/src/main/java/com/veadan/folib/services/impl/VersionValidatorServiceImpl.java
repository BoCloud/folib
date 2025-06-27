package com.veadan.folib.services.impl;

import com.veadan.folib.services.VersionValidatorService;
import com.veadan.folib.storage.validation.ArtifactCoordinatesValidator;

import jakarta.inject.Inject;
import java.util.LinkedHashSet;
import java.util.Set;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component("versionValidatorService")
public class VersionValidatorServiceImpl
        implements VersionValidatorService
{

    @Inject
    private Set<ArtifactCoordinatesValidator> versionValidators = new LinkedHashSet<>();


    public VersionValidatorServiceImpl()
    {
    }

    @Override
    public Set<ArtifactCoordinatesValidator> getVersionValidators()
    {
        return versionValidators;
    }

    public void setVersionValidators(Set<ArtifactCoordinatesValidator> versionValidators)
    {
        this.versionValidators = versionValidators;
    }

}
