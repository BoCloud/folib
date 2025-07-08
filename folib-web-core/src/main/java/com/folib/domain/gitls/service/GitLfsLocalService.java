package com.folib.domain.gitls.service;

import com.folib.domain.gitls.model.*;
import org.springframework.http.ResponseEntity;

import java.io.IOException;

public interface GitLfsLocalService {

    /**
     * 创建新锁
     * @param storageId 存储 ID
     * @param repositoryId  仓库ID
     * @param createLockJson 创建锁对象
     * @return 锁信息
     */
    GitLfsLock.Root createNewLock(String storageId, String repositoryId, GitLfsCreateLock createLockJson);

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
    GitLfsLockList listLocks(String storageId, String repositoryId, String path, String id, int cursor, int limit, String refSpec);

    /**
     * 删除锁
     * @param storageId 存储ID
     * @param repositoryId 仓库ID
     * @param deleteLockRequest 删除锁对象
     * @param lockId 锁ID
     * @return 锁信息
     */
    GitLfsLock.Root deleteLock(String storageId, String repositoryId, GitLfsDeleteLock deleteLockRequest, String lockId);

    /**
     * 锁列表验证
     * @param storageId 存储ID
     * @param repositoryId 仓库ID
     * @param locksVerificationRequest 锁验证对象
     * @return 锁验证信息列表
     */
    GitLfsLocksVerificationList listLocksForVerification(String storageId, String repositoryId, GitLfsLocksVerification locksVerificationRequest);


    /**
     * 下载文件
     * @param storageId 存储ID
     * @param repositoryId 仓库ID
     * @param oid oid
     * @param authHeader 授权头
     * @return GitLfsJson
     */
    ResponseEntity<?> lfsDownloadResponse(String storageId, String repositoryId, String oid, String authHeader)throws IOException;

    /**
     * 验证文件
     * @param storageId 存储ID
     * @param repositoryId 仓库ID
     * @param oid oid
     * @return GitLfsJson
     */
    ResponseEntity<?> lfsVerifyObject(String storageId, String repositoryId, String oid);

    /**
     * lfs 上传响应
     * @param storageId 存储ID
     * @param repositoryId 仓库ID
     * @param lfsJson 上传文件对象
     * @param authHeader 授权头
     * @return GitLfsJson
     */
    ResponseEntity<?> lfsUploadResponse(String storageId, String repositoryId, GitLfsJson lfsJson, String authHeader);
}
