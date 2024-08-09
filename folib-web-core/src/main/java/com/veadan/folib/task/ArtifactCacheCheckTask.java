package com.veadan.folib.task;


import cn.hutool.core.date.StopWatch;
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
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import javax.inject.Inject;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

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

    //@Scheduled(cron = "0 0 3 * * ? ")
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
            StopWatch stopWatch2 = new StopWatch();
            stopWatch2.start("ArtifactCacheCheckTask-2");
            log.info("TotalPages [{}] currentPage [{}] batchSize [{}]", totalPages, currentPage, batchSize);
            List<ArtifactCacheRecord> artifactCacheRecordList = artifactCacheRecordService.getArtifactCacheRecord(null, currentPage, batchSize);
            if (CollectionUtils.isEmpty(artifactCacheRecordList)) {
                continue;
            }
            for (ArtifactCacheRecord artifactCacheRecord : artifactCacheRecordList) {
                StopWatch stopWatch3 = new StopWatch();
                stopWatch3.start("ArtifactCacheCheckTask-3");
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
                stopWatch3.stop();
                log.info(stopWatch3.prettyPrint(TimeUnit.SECONDS));
            }
            stopWatch2.stop();
            log.info(stopWatch2.prettyPrint(TimeUnit.SECONDS));
        }
        log.info("Scheduled ArtifactCacheCheckTask end");
    }

    /**
     * 运行V2版本的处理逻辑
     * 该方法首先获取artifact缓存记录的总数，如果没有记录，则直接返回
     * 接着，它根据总数计算出需要处理的总页数，并使用Flux进行异步并行处理
     * 每一页的数据都会被单独处理，并通过并行调度器运行
     * 最后，记录处理结束的信息
     */
    @Scheduled(cron = "0 0 3 * * ? ")
    public void runV2() {

        // 获取artifact缓存记录的总数
        int totalCount = artifactCacheRecordService.getArtifactCacheRecordCount(null);
        // 如果没有记录，则直接返回
        if (totalCount <= 0) {
            return;
        }

        // 定义每页处理的数量
        int batchSize = 600;
        // 计算总页数，使用向上取整确保能覆盖所有记录
        int totalPages = (int) Math.ceil((double) totalCount / batchSize);

        // 使用Flux.range生成一个表示页面编号的Flux，从1到总页数
        Flux.range(1, totalPages)
                // 对每一页的数据进行并行处理
                .flatMap(page -> Flux.fromIterable(artifactCacheRecordService.getArtifactCacheRecord(null, page, batchSize))
                        // 过滤掉无效的工件路径
                        .filter(this::isValidArtifactPath)
                        // 将过滤后的工件缓存记录聚合成批处理
                        .buffer()
                        //// 在boundedElastic调度器上运行，以弹性方式处理任务 Schedulers.boundedElastic(): 用于处理可能会阻塞的任务，如 I/O 操作。它具有动态扩展的线程池，能够处理大量的阻塞操作。
                        .publishOn(Schedulers.boundedElastic())
                        //并行处理，每次处理2个批次 控制并发的数量
                        .parallel(2)
                        // 处理每个工件缓存记录
                        .doOnNext(this::processArtifactCacheRecord)
                        // 转换回顺序流以确保顺序执行
                        .sequential())
                // 订阅流，触发处理逻辑
                .subscribe();
        // 记录处理结束的信息
        log.info("Scheduled ArtifactCacheCheckTask end");
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
            // 解析制品的仓库路径
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(record.getStorageId(), record.getRepositoryId(), record.getArtifactPath());
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
     * 批量处理工件缓存记录
     * 主要目的是批量删除工件缓存记录，以确保缓存的有效性和更新
     *
     * @param records 待处理的工件缓存记录列表，列表中的每个元素代表一个工件缓存记录
     */
    private void processArtifactCacheRecord(List<ArtifactCacheRecord> records) {
        artifactCacheRecordService.batchDeleteArtifactCacheRecord(records);
    }
}
