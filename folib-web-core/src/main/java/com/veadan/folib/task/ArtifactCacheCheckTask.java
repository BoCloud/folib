package com.veadan.folib.task;


import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactCacheRecordService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
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
import java.util.Objects;
import java.util.stream.Collectors;

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

    @Inject
    @Lazy
    private ConfigurationManager configurationManager;

    /**
     * runV2
     */
    @Scheduled(cron = "0 0 3 * * ? ")
    public void runV2() {
        log.info("Scheduled ArtifactCacheCheckTask start");
        // 获取artifact缓存记录的总数
        int totalCount = artifactCacheRecordService.getArtifactCacheRecordCount(null);
        // 如果没有记录，则直接返回
        if (totalCount <= 0) {
            log.info("Scheduled ArtifactCacheCheckTask end");
            return;
        }
        log.info("Scheduled ArtifactCacheCheckTask totalCount [{}]", totalCount);
        processRecordsByCursor(null);
        log.info("Scheduled ArtifactCacheCheckTask end");
    }

    private void processRecordsByCursor(Long lastId) {
        final int batchSize = 600;
        while (true) {
            long start = System.currentTimeMillis();
            List<ArtifactCacheRecord> records = artifactCacheRecordService.getArtifactCacheRecordByCursor(lastId, batchSize);
            log.debug("LastId [{}] take time [{}] ms", lastId, System.currentTimeMillis() - start);
            if (CollectionUtils.isEmpty(records)) {
                log.info("Scheduled ArtifactCacheCheckTask empty end");
                break;
            }
            List<ArtifactCacheRecord> validRecords = records.stream()
                    .filter(this::isValidArtifactPath)
                    .collect(Collectors.toList());
            if (validRecords.isEmpty()) {
                log.debug("All records were invalid, fetching next batch");
            } else {
                log.debug("Processing valid records, batch size: [{}]", validRecords.size());
                processArtifactCacheRecord(validRecords);
            }
            // 更新 lastId 为当前批次的最后一个记录的ID
            lastId = records.get(records.size() - 1).getId();
        }
    }

    /**
     * 检查制品缓存路径是否有效
     * 该方法用于确定制品缓存记录是否指向一个存在的路径，如果路径不存在或缓存路径为空或不存在，则标记为无效
     *
     * @param record 缓存的制品记录，包含存储ID、仓库ID和制品路径等信息
     * @return 如果制品路径或缓存路径无效，返回true；否则返回false
     */
    private boolean isValidArtifactPath(ArtifactCacheRecord record) {
        // 初始化删除标志为false，代表路径默认有效
        boolean delFlag = false;
        try {
            String storageId = record.getStorageId(), repositoryId = record.getRepositoryId();
            Storage storage = configurationManager.getConfiguration().getStorage(record.getStorageId());
            if (Objects.isNull(storage)) {
                log.warn("Storage [{}] not found", storageId);
                return true;
            }
            Repository repository = storage.getRepository(repositoryId);
            if (Objects.isNull(repository)) {
                log.warn("Repository [{}] [{}] not found", storageId, repositoryId);
                return true;
            }
            // 解析制品的仓库路径
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, record.getArtifactPath());
            // 检查仓库路径是否存在，或者缓存路径是否为空或存在
            delFlag = !Files.exists(repositoryPath) || StringUtils.isBlank(record.getCachePath()) || !Files.exists(Path.of(record.getCachePath()));
        } catch (Exception ex) {
            // 日志记录检查过程中的异常情况
            log.warn("制品缓存检查，执行失败，缓存制品 [{}] [{}]", JSONObject.toJSONString(record), ExceptionUtils.getStackTrace(ex));
        }
        // 返回路径是否无效的结果
        return delFlag;
    }

    /**
     * 批量处理制品缓存记录
     * 主要目的是批量删除制品缓存记录，以确保缓存的有效性和更新
     *
     * @param records 待处理的制品缓存记录列表，列表中的每个元素代表一个制品缓存记录
     */
    private void processArtifactCacheRecord(List<ArtifactCacheRecord> records) {
        artifactCacheRecordService.batchDeleteArtifactCacheRecord(records);
    }
}
