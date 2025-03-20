package com.veadan.folib.services;

import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RepositoryManagementStrategyException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.RepositoryPermissionDto;

import java.io.IOException;
import java.util.Map;

/**
 * @author mtodorov
 */
public interface RepositoryManagementService {

    void createRepository(String storageId,
                          String repositoryId)
            throws IOException, RepositoryManagementStrategyException;

    void removeRepository(String storageId,
                          String repositoryId)
            throws IOException;

    void deleteTrash(String storageId, String repositoryId, String storageDay, Map<String, String> cleanupArtifactPathMap)
            throws IOException;

    void deleteTrash(boolean checkTask, String storageDa, Map<String, String> cleanupArtifactPathMap)
            throws IOException;

    void undelete(RepositoryPath repositoryPath)
            throws IOException;

    void undeleteTrash(String storageId, String repositoryId)
            throws IOException,
            ProviderImplementationException;

    void undeleteTrash()
            throws IOException,
            ProviderImplementationException;

    void putInService(String storageId, String repositoryId) throws IOException;

    void putOutOfService(String storageId, String repositoryId) throws IOException;

    Storage getStorage(String storageId);

    /**
     * 处理仓库级别权限
     *
     * @param storageId               存储空间名称
     * @param repositoryId            仓库名称
     * @param repositoryPermissionDto 参数
     */
    void handlerRepositoryPermission(String storageId, String repositoryId, RepositoryPermissionDto repositoryPermissionDto);

    /**
     * 删除用户仓库权限
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param username     用户名
     * @param permissions  权限
     */
    void deleteRepositoryPermission(String storageId, String repositoryId, String username, String permissions);

}
