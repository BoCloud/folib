package com.folib.services;

import com.folib.repository.RepositoryManagementStrategyException;
import com.folib.storage.Storage;
import com.folib.storage.repository.RepositoryPermissionDto;

import java.io.IOException;

/**
 * @author veadan
 */
public interface RepositoryManagementService {

    void createRepository(String storageId,
                          String repositoryId)
            throws IOException, RepositoryManagementStrategyException;

    void removeRepository(String storageId,
                          String repositoryId)
            throws IOException;

    void cleanupRepository(String storageId,
                           String repositoryId)
            throws IOException;

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

    /**
     * 删除仓库下的空目录
     */
    void deleteEmptyDirectory();

    /**
     * 删除仓库下的空目录
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     */
    void deleteEmptyDirectory(String storageId, String repositoryId);

}
