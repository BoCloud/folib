package com.veadan.folib.ws.server;

import com.veadan.folib.config.PromotionConfig;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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

//    @PostConstruct
//    public void init() {
//        final Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
//        clusterDispatchNode.values().forEach((nodeInfo) -> {
//            String targetHostName = FolibWsRunManageUtil.getTargetHostName(nodeInfo);
//            registerPromotionTaskQueue(targetHostName);
//        });
//    }

    public TaskQueueManager getTaskQueueManager(String targetHostName) {
        return PROMOTION_TASK_QUEUE.get(targetHostName);
    }

    public void registerPromotionTaskQueue(String targetHostName) {
        log.info("registerPromotionTaskQueue {}",targetHostName);
        TaskQueueManager taskQueueManager = new TaskQueueManager("promotion_to_" + targetHostName, promotionConfig.getQueueSize(), promotionConfig.getThread());
        PROMOTION_TASK_QUEUE.putIfAbsent(targetHostName, taskQueueManager);
    }

    /**
     * 节点被手动删除时执行清理队列
     * @param targetHostName
     */
    public void clearPromotionTaskQueue(String targetHostName) {
        log.info("clearPromotionTaskQueue {}",targetHostName);
        TaskQueueManager remove = PROMOTION_TASK_QUEUE.remove(targetHostName);
        if (remove != null) {
            remove.shutdownAndCancelTasks();
        }
    }
}
