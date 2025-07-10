/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
