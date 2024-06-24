package com.veadan.folib.services;

import com.veadan.folib.domain.gitls.model.*;
import com.veadan.folib.domain.gitls.service.GitLfsLocalService;
import com.veadan.folib.entity.GitLfsLockEntity;

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
