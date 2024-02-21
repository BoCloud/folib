package com.veadan.folib.ws.server;

import com.veadan.folib.config.PromotionConfig;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author pengYongQiang
 * @date 2024/2/20 16:47
 */
@Component
public class PromotionTaskQueue {
    private ConcurrentHashMap<String, TaskQueueManager> PROMOTION_TASK_QUEUE = new ConcurrentHashMap<>();


    @Autowired
    private ConfigurationManagementService configurationManagementService;
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;

    @Autowired
    private PromotionConfig promotionConfig;

    @PostConstruct
    public void init() {
        // 初始化连接到集群服务端
        final Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
        clusterDispatchNode.values().stream()
                // 排除自动注册的节点信息
                .filter(e -> null != e.getAutoRegister() && !e.getAutoRegister())
                .forEach((nodeInfo) -> {
                    String targetHostName = folibWsRunManageV2.getTargetHostName(nodeInfo);
                    registerPromotionTaskQueue(targetHostName);
                });
    }

    public TaskQueueManager getTaskQueueManager(String targetHostName) {
        return PROMOTION_TASK_QUEUE.get(targetHostName);
    }

    public void registerPromotionTaskQueue(String targetHostName) {
        TaskQueueManager taskQueueManager = new TaskQueueManager("promotion_to_" + targetHostName, promotionConfig.getQueueSize());
        PROMOTION_TASK_QUEUE.putIfAbsent(targetHostName, taskQueueManager);
    }

    public void clearPromotionTaskQueue(String targetHostName) {
        TaskQueueManager remove = PROMOTION_TASK_QUEUE.remove(targetHostName);
        if (remove != null) {
            remove.shutdownAndCancelTasks();
        }
    }
}
