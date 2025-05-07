package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.veadan.folib.entity.GitLfsLockEntity;
import com.veadan.folib.mapper.GitLfsLockMapper;
import com.veadan.folib.services.GitLfsLocalLockService;
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
