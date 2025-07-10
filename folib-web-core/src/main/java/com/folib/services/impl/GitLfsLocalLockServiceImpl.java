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
package com.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.folib.entity.GitLfsLockEntity;
import com.folib.mapper.GitLfsLockMapper;
import com.folib.services.GitLfsLocalLockService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.util.List;
import java.util.UUID;

@Slf4j
@Service
public class GitLfsLocalLockServiceImpl implements GitLfsLocalLockService {
    @Inject
    private GitLfsLockMapper gitLfsLockMapper;

    /**
     * 创建新锁
     *
     * @param entity 锁对象
     * @return 锁对象
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public GitLfsLockEntity createNewLock(GitLfsLockEntity entity) {
        entity.setId(UUID.randomUUID().toString());
        try {
            gitLfsLockMapper.insert(entity);
        } catch (Exception ex) {
            throw ex;
        }
        return entity;
    }

    /**
     * 查询锁
     *
     * @param entity 锁对象
     * @return 锁对象
     */
    @Override
    public GitLfsLockEntity getOneLock(GitLfsLockEntity entity) {
        return gitLfsLockMapper.selectOne(Wrappers.<GitLfsLockEntity>lambdaQuery()
                .eq(GitLfsLockEntity::getPath, entity.getPath())
                .eq(GitLfsLockEntity::getOwner, entity.getOwner())
                .eq(GitLfsLockEntity::getRef, entity.getRef())
        );
    }

    /**
     * 查询锁列表
     *
     * @param storageId    存储 ID
     * @param repositoryId 仓库ID
     * @param path         锁路径
     * @param id           锁ID
     * @param cursor       游标
     * @param limit        锁数量的限制
     * @param refSpec      从中搜索锁
     * @return 锁列表
     */
    @Override
    public List<GitLfsLockEntity> listLocks(String storageId, String repositoryId, String path, String id, int cursor, int limit, String refSpec) {
        if (limit <= 0) {
            limit = Math.toIntExact(gitLfsLockMapper.selectCount(Wrappers.<GitLfsLockEntity>lambdaQuery()
                    .eq(GitLfsLockEntity::getPath, path)
                    .eq(GitLfsLockEntity::getRef, refSpec)
                    .eq(GitLfsLockEntity::getRepositoryId, repositoryId)
                    .eq(GitLfsLockEntity::getStorageId, storageId)
                    .eq(GitLfsLockEntity::getId, id)
            ));
        }
        return gitLfsLockMapper.queryAllByLimit(storageId, repositoryId, path, id, cursor, limit, refSpec);
    }

    /**
     * 删除锁
     *
     * @param entity 删除锁对象
     * @return 锁信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public GitLfsLockEntity deleteLock(GitLfsLockEntity entity) {
        GitLfsLockEntity lfsLock = gitLfsLockMapper.selectOne(Wrappers.<GitLfsLockEntity>lambdaQuery()
                .eq(GitLfsLockEntity::getId, entity.getId())
                .eq(GitLfsLockEntity::getRepositoryId, entity.getRepositoryId()).eq(GitLfsLockEntity::getStorageId, entity.getStorageId())
                .eq(GitLfsLockEntity::getPath, entity.getPath())
                .eq(GitLfsLockEntity::getRef, entity.getRef())
        );
        gitLfsLockMapper.deleteById(entity.getId());
        return lfsLock;
    }
}
