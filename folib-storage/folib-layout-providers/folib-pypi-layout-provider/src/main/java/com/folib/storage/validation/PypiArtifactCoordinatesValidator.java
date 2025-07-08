package com.folib.storage.validation;

import com.folib.providers.layout.PypiLayoutProvider;
import com.folib.storage.repository.Repository;

import java.util.Set;

import com.google.common.collect.Sets;

/**
 * @author sainalshah
 */
public interface PypiArtifactCoordinatesValidator
        extends ArtifactCoordinatesValidator
{

    @Override
    default boolean supports(Repository repository)
    {
        return supports(repository.getLayout());
    }

    @Override
    default boolean supports(String layoutProvider)
    {
        return PypiLayoutProvider.ALIAS.equals(layoutProvider);
    }

    @Override
    default Set<String> getSupportedLayoutProviders()
    {
        return Sets.newHashSet(PypiLayoutProvider.ALIAS);
    }

}
