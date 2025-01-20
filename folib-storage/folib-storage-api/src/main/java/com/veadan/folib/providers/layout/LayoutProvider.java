package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactGroup;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nonnull;
import javax.ws.rs.core.MultivaluedMap;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;
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

    default void targetUrl(RepositoryPath path) throws IOException {
        if (Objects.isNull(path) || StringUtils.isBlank(path.getTargetUrl())) {
            return;
        }
        Repository repository = path.getRepository();
        if (!RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            return;
        }
        if (GlobalConstants.HTTP_PREFIX_LIST.stream().anyMatch(item -> path.getTargetUrl().startsWith(item))) {
            return;
        }
        String remoteUrl = repository.getRemoteRepository().getUrl();
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        path.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(path.getTargetUrl(), GlobalConstants.SEPARATOR)));
    }

    default int refreshContentInterval(RepositoryPath repositoryPath) {
        return GlobalConstants.DEFAULT_REFRESH_CONTENT_INTERVAL;
    }
}
