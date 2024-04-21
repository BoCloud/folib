package com.veadan.folib.ws.server;

import com.veadan.folib.config.PromotionConfig;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author pengYongQiang
 * @date 2024/2/20 16:47
 */
@Component
@Slf4j
public class PromotionTaskQueue {
    private ConcurrentHashMap<String, TaskQueueManager> PROMOTION_TASK_QUEUE = new ConcurrentHashMap<>();


    @Autowired
    private ConfigurationManagementService configurationManagementService;
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;

    @Autowired
    private PromotionConfig promotionConfig;

    public TargetTaskQueueManager getTaskQueueManager(String targetHostName) {
        TargetTaskQueueManager targetTaskQueueManager = null;
        TaskQueueManager taskQueueManager = PROMOTION_TASK_QUEUE.get(targetHostName);
        if (Objects.isNull(taskQueueManager)) {
            targetHostName = getTargetHostName(targetHostName);
            taskQueueManager = PROMOTION_TASK_QUEUE.get(targetHostName);
        }
        if (Objects.nonNull(taskQueueManager)) {
            targetTaskQueueManager = TargetTaskQueueManager.builder().taskQueueManager(taskQueueManager).targetHostName(targetHostName).build();
            log.info("Find targetTaskQueueManager for targetHostName [{}]", targetHostName);
        }
        return targetTaskQueueManager;
    }

    public void registerPromotionTaskQueue(String targetHostName) {
        log.info("registerPromotionTaskQueue {}", targetHostName);
        TaskQueueManager taskQueueManager = new TaskQueueManager("promotion_to_" + targetHostName, promotionConfig.getQueueSize(), promotionConfig.getThread());
        PROMOTION_TASK_QUEUE.putIfAbsent(targetHostName, taskQueueManager);
    }

    /**
     * 节点被手动删除时执行清理队列
     *
     * @param targetHostName
     */
    public void clearPromotionTaskQueue(String targetHostName) {
        log.info("clearPromotionTaskQueue {}", targetHostName);
        TaskQueueManager remove = PROMOTION_TASK_QUEUE.remove(targetHostName);
        if (remove != null) {
            remove.shutdownAndCancelTasks();
        }
    }

    private String getTargetHostName(String targetHostName) {
        String targetHostNamePrefix = targetHostName.split("_")[0];
        if (MapUtils.isNotEmpty(PROMOTION_TASK_QUEUE)) {
            Optional<String> optional = PROMOTION_TASK_QUEUE.keySet().stream().filter(item -> item.startsWith(targetHostNamePrefix)).findFirst();
            if (optional.isPresent()) {
                return optional.get();
            }
        }
        return targetHostName;
    }

}
