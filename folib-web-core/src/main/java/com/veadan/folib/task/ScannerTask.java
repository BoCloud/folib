package com.veadan.folib.task;


import cn.hutool.core.date.DateUtil;
import com.google.common.collect.Lists;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.scanner.service.ScanService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.owasp.dependencycheck.utils.Checksum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 扫描task
 */
@Slf4j
@Component
@EnableScheduling
public class ScannerTask {

    @Inject
    private ArtifactRepository artifactRepository;

    @Autowired
    private ScanService scanService;

    @Autowired
    private ScanRulesMapper scanRulesMapper;

    @Autowired
    private DistributedLockComponent distributedLockComponent;

    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        String lockName = "ScannerTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                Example example = new Example(ScanRules.class);
                example.createCriteria().andEqualTo("onScan", 1);
                List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
                if (CollectionUtils.isEmpty(scanRulesList)) {
                    return;
                }
                List<String> storageIdAndRepositoryIdList = scanRulesList.stream().map(item -> String.format("%s-%s", item.getStorage(), item.getRepository())).collect(Collectors.toList());
                List<String> safeLevels = Lists.newArrayList();
                safeLevels.add(SafeLevelEnum.INIT.getLevel());
                safeLevels.add(SafeLevelEnum.SCANNING.getLevel());
                safeLevels.add(SafeLevelEnum.SCAN_FAIL.getLevel());
                safeLevels.add(SafeLevelEnum.UN_SCAN.getLevel());
                List<Artifact> artifactList = artifactRepository.findMatchingBySafeLevels(storageIdAndRepositoryIdList, safeLevels);
                if (CollectionUtils.isNotEmpty(artifactList)) {
                    int size = 50;
                    List<List<Artifact>> lists = Lists.partition(artifactList, size);
                    for (List<Artifact> itemList : lists) {
                        scanService.asyncScan(itemList);
                    }
                }
                Checksum.clearCache();
                log.info("ScannerTask thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }
}
