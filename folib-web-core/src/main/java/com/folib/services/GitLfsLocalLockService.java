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

import com.folib.entity.GitLfsLockEntity;

import java.util.List;

public interface GitLfsLocalLockService {


    /**
     * 创建新锁
     * @param entity 锁对象
     * @return 锁对象
     */
    GitLfsLockEntity createNewLock(GitLfsLockEntity entity);

    /**
     * 查询锁
     * @param entity 锁对象
     * @return 锁对象
     */
    GitLfsLockEntity getOneLock(GitLfsLockEntity entity);

    /**
     *  查询锁列表
     * @param storageId 存储 ID
     * @param repositoryId 仓库ID
     * @param path 锁路径
     * @param id 锁ID
     * @param cursor 游标
     * @param limit  锁数量的限制
     * @param refSpec 从中搜索锁
     * @return 锁列表
     */
    List<GitLfsLockEntity> listLocks(String storageId, String repositoryId, String path, String id, int cursor, int limit, String refSpec);

    /**
     * 删除锁
     * @param entity 删除锁对象
     * @return 锁信息
     */
    GitLfsLockEntity deleteLock(GitLfsLockEntity entity);

}
