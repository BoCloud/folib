package com.veadan.folib.scanner.config;


import cn.hutool.core.date.DateUtil;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
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
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.List;
import java.util.stream.Collectors;

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
    private ScanRulesMapper scanRulesMapper;

    @Autowired
    private FolibDistributedSchedulerLock folibDistributedSchedulerLock;

    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        logger.info("Wait for the lock [folib.ScannerTask]");
        if (folibDistributedSchedulerLock.getLock("folib.ScannerTask", 300L)) {
            logger.info("Get lock [folib.ScannerTask]");
            Example example = new Example(ScanRules.class);
            example.createCriteria().andEqualTo("onScan", 1);
            List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
            if (CollectionUtils.isEmpty(scanRulesList)) {
                return;
            }
            List<String> storageIdAndRepositoryIdList = scanRulesList.stream().map(item -> String.format("%s-%s", item.getStorage(), item.getRepository())).collect(Collectors.toList());
            List<String> safeLevels = Lists.newArrayList();
            safeLevels.add(SafeLevelEnum.SCANNING.getLevel());
            safeLevels.add(SafeLevelEnum.SCAN_FAIL.getLevel());
            safeLevels.add(SafeLevelEnum.UN_SCAN.getLevel());
            List<Artifact> artifactList = artifactRepository.findMatchingBySafeLevels(storageIdAndRepositoryIdList, safeLevels);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                artifactList.forEach(artifact -> scanService.asyncScan(artifact));
            } 
            logger.info("=====>>>>>当前线程名称：{}，使用cron异步执行：{}", Thread.currentThread().getName(), DateUtil.now());
        }
        logger.info("ScannerTask end");
    }
}
