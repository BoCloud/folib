package com.veadan.folib.scanner.config;


import cn.hutool.core.date.DateUtil;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.service.ScanService;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.services.FolibDistributedSchedulerLock;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.List;

@Component
@EnableScheduling
public class ScannerTask {
    private static final Logger logger = LoggerFactory.getLogger(
            ScannerTask.class);

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private ArtifactService artifactService;

    @Autowired
    private ScanService scanService;

    @Autowired
    private FolibDistributedSchedulerLock folibDistributedSchedulerLock;

    @Scheduled(cron = "0 0/1 * * * ? ")
    public void run() {
        logger.info("Wait for the lock [folib.ScannerTask]");
        if (folibDistributedSchedulerLock.getLock("folib.ScannerTask", 300L)) {
            logger.info("Get lock [folib.ScannerTask]");
            //将正在扫描中的变为失败
            List<String> safeLevels = Lists.newArrayList();
            safeLevels.add(SafeLevelEnum.SCANNING.getLevel());
            List<Artifact> artifactList = artifactRepository.findMatchingBySafeLevels(safeLevels);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                for (Artifact artifact : artifactList) {
                    artifact.setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
                    artifactService.saveOrUpdateArtifact(artifact);
                }
            }
            safeLevels = Lists.newArrayList();
            safeLevels.add(SafeLevelEnum.SCAN_FAIL.getLevel());
            safeLevels.add(SafeLevelEnum.UN_SCAN.getLevel());
            artifactList = artifactRepository.findMatchingBySafeLevels(safeLevels);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                artifactList.forEach(artifact -> scanService.asyncScan(artifact));
            }
            logger.info("=====>>>>>当前线程名称：{}，使用cron异步执行：{}", Thread.currentThread().getName(), DateUtil.now());
        }
        logger.info("ScannerTask end");
    }
}
