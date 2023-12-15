package com.veadan.folib.task;


import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactCacheRecordService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.nio.file.Files;
import java.util.List;

/**
 * @author leipenghui
 * 制品缓存检查task
 */
@Slf4j
@Component
@EnableScheduling
public class ArtifactCacheCheckTask {

    @Inject
    @Lazy
    private ArtifactCacheRecordService artifactCacheRecordService;

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Scheduled(cron = "0 0 3 * * ? ")
    public void run() {
        String lockName = "ArtifactCacheCheckTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                int totalCount = artifactCacheRecordService.getArtifactCacheRecordCount(null);
                if (totalCount <= 0) {
                    return;
                }
                int batchSize = 1000;
                // 计算总页数
                int totalPages = (int) Math.ceil((double) totalCount / batchSize);
                for (int currentPage = 1; currentPage <= totalPages; currentPage++) {
                    log.info("CurrentPage [{}] batchSize [{}]", currentPage, batchSize);
                    List<ArtifactCacheRecord> artifactCacheRecordList = artifactCacheRecordService.getArtifactCacheRecord(null, currentPage, batchSize);
                    if (CollectionUtils.isEmpty(artifactCacheRecordList)) {
                        continue;
                    }
                    RepositoryPath repositoryPath;
                    for (ArtifactCacheRecord artifactCacheRecord : artifactCacheRecordList) {
                        try {
                            repositoryPath = repositoryPathResolver.resolve(artifactCacheRecord.getStorageId(), artifactCacheRecord.getRepositoryId(), artifactCacheRecord.getArtifactPath());
                            if (!Files.exists(repositoryPath)) {
                                //源制品不存在，删除缓存记录
                                artifactCacheRecordService.deleteArtifactCacheRecord(artifactCacheRecord);
                            }
                        } catch (Exception ex) {
                            log.warn("制品缓存检查，执行失败，缓存制品 [{}] [{}]", JSONObject.toJSONString(artifactCacheRecord), ExceptionUtils.getStackTrace(ex));
                        }
                    }
                }
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
        log.info("Scheduled ArtifactCacheCheckTask end");
    }
}
