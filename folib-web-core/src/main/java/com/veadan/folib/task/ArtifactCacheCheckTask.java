package com.veadan.folib.task;


import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactCacheRecordService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.nio.file.Files;
import java.nio.file.Path;
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
        int totalCount = artifactCacheRecordService.getArtifactCacheRecordCount(null);
        if (totalCount <= 0) {
            return;
        }
        int batchSize = 600;
        // 计算总页数
        int totalPages = (int) Math.ceil((double) totalCount / batchSize);
        RepositoryPath repositoryPath;
        boolean delFlag = false;
        for (int currentPage = 1; currentPage <= totalPages; currentPage++) {
            log.info("TotalPages [{}] currentPage [{}] batchSize [{}]", totalPages, currentPage, batchSize);
            List<ArtifactCacheRecord> artifactCacheRecordList = artifactCacheRecordService.getArtifactCacheRecord(null, currentPage, batchSize);
            if (CollectionUtils.isEmpty(artifactCacheRecordList)) {
                continue;
            }
            for (ArtifactCacheRecord artifactCacheRecord : artifactCacheRecordList) {
                try {
                    repositoryPath = repositoryPathResolver.resolve(artifactCacheRecord.getStorageId(), artifactCacheRecord.getRepositoryId(), artifactCacheRecord.getArtifactPath());
                    // 源制品不存在、缓存文件不存在，删除缓存记录
                    delFlag = !Files.exists(repositoryPath) || StringUtils.isBlank(artifactCacheRecord.getCachePath()) || !Files.exists(Path.of(artifactCacheRecord.getCachePath()));
                    if (delFlag) {
                        artifactCacheRecordService.deleteArtifactCacheRecord(artifactCacheRecord);
                    }
                } catch (Exception ex) {
                    log.warn("制品缓存检查，执行失败，缓存制品 [{}] [{}]", JSONObject.toJSONString(artifactCacheRecord), ExceptionUtils.getStackTrace(ex));
                }
            }
        }
        log.info("Scheduled ArtifactCacheCheckTask end");
    }
}
