package com.veadan.folib.providers.layout;

import java.io.IOException;
import java.util.Set;

import javax.annotation.Nonnull;

import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.domain.ArtifactGroup;
import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author Veadan
 */
public interface LayoutProvider<T extends ArtifactCoordinates>
{
    RepositoryManagementStrategy getRepositoryManagementStrategy();

    @Nonnull
    Set<String> listArchiveFilenames(RepositoryPath repositoryPath);

    Set<String> getDefaultArtifactCoordinateValidators();

    String getAlias();

    @Nonnull
    Set<ArtifactGroup> getArtifactGroups(RepositoryPath path)
            throws IOException;

}
