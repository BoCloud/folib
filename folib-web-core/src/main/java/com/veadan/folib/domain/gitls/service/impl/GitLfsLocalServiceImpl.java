package com.veadan.folib.domain.gitls.service.impl;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.gitls.command.local.*;
import com.veadan.folib.domain.gitls.model.*;

import com.veadan.folib.domain.gitls.service.GitLfsLocalService;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.GitLfsLocalLockService;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.IOException;

@Service
public class GitLfsLocalServiceImpl implements GitLfsLocalService {

    @Inject
    private GitLfsLocalLockService gitLfsLocalLockService;
    @Inject
    private ConfigurationManager configurationManager;
    @Inject
    private ArtifactRepository artifactRepository;
    @Inject
    private ArtifactResolutionService artifactResolutionServic;


    /**
     * 创建新锁
     *
     * @param storageId      存储 ID
     * @param repositoryId   仓库ID
     * @param createLockJson 创建锁对象
     * @return 锁信息
     */
    @Override
    public GitLfsLock.Root createNewLock(String storageId, String repositoryId, GitLfsCreateLock createLockJson) {
        return new LocalCreateLockCommand(gitLfsLocalLockService).createNewLock(storageId, repositoryId, createLockJson);
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
    public GitLfsLockList listLocks(String storageId, String repositoryId, String path, String id, int cursor, int limit, String refSpec) {
        return new LocalListLockCommand(gitLfsLocalLockService).listLocks(storageId, repositoryId, path, id, cursor, limit, refSpec);
    }

    /**
     * 删除锁
     *
     * @param storageId         存储ID
     * @param repositoryId      仓库ID
     * @param deleteLockRequest 删除锁对象
     * @param lockId            锁ID
     * @return 锁信息
     */
    @Override
    public GitLfsLock.Root deleteLock(String storageId, String repositoryId, GitLfsDeleteLock deleteLockRequest, String lockId) {
        return new LocalDeleteLockCommand(gitLfsLocalLockService).lfsDeleteLock(storageId, repositoryId, deleteLockRequest, lockId);
    }

    /**
     * 锁列表验证
     *
     * @param storageId                存储ID
     * @param repositoryId             仓库ID
     * @param locksVerificationRequest 锁验证对象
     * @return 锁验证信息列表
     */
    @Override
    public GitLfsLocksVerificationList listLocksForVerification(String storageId, String repositoryId, GitLfsLocksVerification locksVerificationRequest) {
        return new LocalListLockVerificationCommand(gitLfsLocalLockService).listLocksForVerification(storageId, repositoryId, locksVerificationRequest);
    }

    /**
     * 下载文件
     *
     * @param storageId    存储ID
     * @param repositoryId 仓库ID
     * @param oid          oid
     * @param authHeader   授权头
     * @return GitLfsJson
     */
    @Override
    public ResponseEntity<?> lfsDownloadResponse(String storageId, String repositoryId, String oid, String authHeader)throws IOException {
        return new LocalDownloadCommand(configurationManager, artifactRepository, artifactResolutionServic).download(storageId, repositoryId, oid, authHeader);
    }

    /**
     * 验证文件
     *
     * @param storageId    存储ID
     * @param repositoryId 仓库ID
     * @param oid          oid
     * @return GitLfsJson
     */
    @Override
    public ResponseEntity<?> lfsVerifyObject(String storageId, String repositoryId, String oid) {
        return new LocalVerifyCommand(configurationManager, artifactRepository).verify(storageId, repositoryId, oid);
    }

    /**
     * lfs 上传响应
     *
     * @param storageId    存储ID
     * @param repositoryId 仓库ID
     * @param lfsJson      上传文件对象
     * @param authHeader   授权头
     * @return GitLfsJson
     */
    @Override
    public ResponseEntity<?> lfsUploadResponse(String storageId, String repositoryId, GitLfsJson lfsJson, String authHeader) {
        return new LocalUploadCommand(artifactRepository, configurationManager).upload(storageId, repositoryId, lfsJson, authHeader);
    }
}
