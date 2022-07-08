package com.veadan.folib.storage.validation;

import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.storage.repository.Repository;

import java.util.Set;

import com.google.common.collect.Sets;

/**
 * @author Veadan
 */
public interface MavenArtifactCoordinatesValidator extends ArtifactCoordinatesValidator
{


    @Override
    default boolean supports(Repository repository)
    {
        return supports(repository.getLayout());
    }

    @Override
    default boolean supports(String layoutProvider)
    {
        return Maven2LayoutProvider.ALIAS.equals(layoutProvider);
    }

    @Override
    default Set<String> getSupportedLayoutProviders()
    {
        return Sets.newHashSet(Maven2LayoutProvider.ALIAS);
    }

}
