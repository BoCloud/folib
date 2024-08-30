package com.veadan.folib.task;


import com.github.pagehelper.PageHelper;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.enums.ArtifactSyncRecordOpsTypeEnum;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.services.ArtifactPromotionService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * 晋级、分发补偿task
 */
@Slf4j
@Component
@EnableScheduling
public class PromotionCompensationTask {

    @Inject
    @Lazy
    private PromotionUtil promotionUtil;

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    @Lazy
    private ArtifactPromotionService artifactPromotionService;

    @Inject
    @Lazy
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;

    /**
     * 每8分钟
     */
    @Scheduled(cron = "0 0/8 * * * ? ")
    public void run() {
        String lockName = "PromotionCompensationTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                Example recordExample = Example.builder(ArtifactSyncRecord.class).build();
                recordExample.and().andEqualTo("status", ArtifactSyncRecordStatusEnum.FAILED.getVal());
                recordExample.and().andLessThan("retryCount", getMaxRetryCount());
                int totalCount = artifactSyncRecordMapper.selectCountByExample(recordExample);
                log.info("Promotion compensation task find total [{}] artifact", totalCount);
                if (totalCount <= 0) {
                    return;
                }
                int batchSize = 10;
                // 计算总页数
                int totalPages = (int) Math.ceil((double) totalCount / batchSize);
                List<ArtifactSyncRecord> artifactSyncRecordList;
                recordExample.setOrderByClause("id asc");
                for (int currentPage = 1; currentPage <= totalPages; currentPage++) {
                    log.info("Promotion compensation task totalPages [{}] currentPage [{}] batchSize [{}]", totalPages, currentPage, batchSize);
                    PageHelper.startPage(currentPage, batchSize);
                    artifactSyncRecordList = artifactSyncRecordMapper.selectByExample(recordExample);
                    if (CollectionUtils.isEmpty(artifactSyncRecordList)) {
                        continue;
                    }
                    for (ArtifactSyncRecord artifactSyncRecord : artifactSyncRecordList) {
                        try {
                            log.info("晋级自动补偿重试，晋级编号 [{}] 晋级类型 [{}] 重试次数 [{}]", artifactSyncRecord.getSyncNo(), artifactSyncRecord.getOpsType(), Objects.nonNull(artifactSyncRecord.getRetryCount()) ? artifactSyncRecord.getRetryCount() + 1 : 1);
                            if (ArtifactSyncRecordOpsTypeEnum.PROMOTION.getVal().equals(artifactSyncRecord.getOpsType())) {
                                artifactPromotionService.retryNodeOptionAttachRecord(artifactSyncRecord.getSyncNo(), null);
                            } else if (ArtifactSyncRecordOpsTypeEnum.DISPATCH.getVal().equals(artifactSyncRecord.getOpsType())) {
                                promotionUtil.executeHandleRetryDispatch(artifactSyncRecord.getSyncNo());
                            }
                        } catch (Exception ex) {
                            log.warn("晋级自动补偿重试，晋级编号 [{}] 晋级类型 [{}] 重试次数 [{}] 错误 [{}]", artifactSyncRecord.getSyncNo(), artifactSyncRecord.getOpsType(), artifactSyncRecord.getRetryCount(), ExceptionUtils.getStackTrace(ex));
                        }
                    }
                }
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }

    private Integer getMaxRetryCount() {
        int maxRetryCount = 5;
        String key = "PROMOTION_COMPENSATION_TASK_MAX_RETRY_COUNT";
        String value = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(value)) {
            maxRetryCount = Integer.parseInt(value);
        }
        return maxRetryCount;
    }
}
