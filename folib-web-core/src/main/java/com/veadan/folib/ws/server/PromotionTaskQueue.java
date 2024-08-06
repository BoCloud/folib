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
@Deprecated
public class PromotionTaskQueue {

    private ConcurrentHashMap<String, TaskQueueManager> PROMOTION_TASK_QUEUE = new ConcurrentHashMap<>();

    private ConcurrentHashMap<String, TaskQueueV2Manager> PROMOTION_TASK__V2_QUEUE = new ConcurrentHashMap<>();


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

    public TargetTaskQueueV2Manager getTaskQueueV2Manager(String targetHostName) {
        TargetTaskQueueV2Manager targetTaskQueueManager = null;
        TaskQueueV2Manager taskQueueManager = PROMOTION_TASK__V2_QUEUE.get(targetHostName);
        if (Objects.isNull(taskQueueManager)) {
            targetHostName = getV2TargetHostName(targetHostName);
            taskQueueManager = PROMOTION_TASK__V2_QUEUE.get(targetHostName);
        }
        if (Objects.nonNull(taskQueueManager)) {
            targetTaskQueueManager = TargetTaskQueueV2Manager.builder().taskQueueV2Manager(taskQueueManager).targetHostName(targetHostName).build();
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
     * 注册任务队列
     * @param targetHostName 目标主机名
     */
    public void registerV2PromotionTaskQueue(String targetHostName) {
        log.info("registerPromotionTaskQueue {}", targetHostName);
        TaskQueueV2Manager taskQueueManager = new TaskQueueV2Manager("promotion_to_" + targetHostName, promotionConfig.getQueueSize(), promotionConfig.getThread());
        PROMOTION_TASK__V2_QUEUE.putIfAbsent(targetHostName, taskQueueManager);
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

    /**
     * 节点被手动删除时执行清理队列
     *
     * @param targetHostName
     */
    public void clearV2PromotionTaskQueue(String targetHostName) {
        log.info("clearPromotionTaskQueue {}", targetHostName);
        TaskQueueV2Manager remove = PROMOTION_TASK__V2_QUEUE.remove(targetHostName);
        if (remove != null) {
            remove.shutdownAndCancelTasks();
        }
    }
    public void updateTaskQueuePriority(String targetHostName,String syncNo, Priority priority){
        TaskQueueV2Manager taskQueueV2Manager = PROMOTION_TASK__V2_QUEUE.remove(targetHostName);
        if (Objects.nonNull(taskQueueV2Manager)) {
            taskQueueV2Manager.updateTaskPriority(syncNo, priority.getValue());
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

    private String getV2TargetHostName(String targetHostName) {
        String targetHostNamePrefix = targetHostName.split("_")[0];
        if (MapUtils.isNotEmpty(PROMOTION_TASK__V2_QUEUE)) {
            Optional<String> optional = PROMOTION_TASK__V2_QUEUE.keySet().stream().filter(item -> item.startsWith(targetHostNamePrefix)).findFirst();
            if (optional.isPresent()) {
                return optional.get();
            }
        }
        return targetHostName;
    }

}
