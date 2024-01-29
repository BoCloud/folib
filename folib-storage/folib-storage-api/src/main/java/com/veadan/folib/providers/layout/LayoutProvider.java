package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.domain.ArtifactGroup;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RepositoryManagementStrategy;

import javax.annotation.Nonnull;
import javax.ws.rs.core.MultivaluedMap;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;

/**
 * @author Veadan
 */
public interface LayoutProvider<T extends ArtifactCoordinates> {
    RepositoryManagementStrategy getRepositoryManagementStrategy();

    @Nonnull
    Set<String> listArchiveFilenames(RepositoryPath repositoryPath);

    byte[] getContentByFileName(RepositoryPath repositoryPath, String fileName);

    byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName);

    byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName);

    Set<String> getDefaultArtifactCoordinateValidators();

    String getAlias();

    @Nonnull
    Set<ArtifactGroup> getArtifactGroups(RepositoryPath path)
            throws IOException;

    default void initData(String storageId, String repositoryId) {

    }

    default void targetUrl(RepositoryPath path) {

    }

}
