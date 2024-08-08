package com.veadan.folib.task;


import cn.hutool.core.date.DateUtil;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.dto.UserAuthReq;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.WSMessageResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.Map;

/**
 * @author leipenghui
 * 同步用户权限定时task
 */
@Slf4j
@Component
@EnableScheduling
public class UserAuthSyncTask {

    @Autowired
    private DistributedLockComponent distributedLockComponent;
    @Inject
    @Lazy
    protected ConfigurationManagementService configurationManagementService;
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        String lockName = "UserAuthSyncTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                        getMutableConfigurationClone().getClusterDispatchNode();
                if (MapUtils.isEmpty(map)) {
                    return;
                }
                map.forEach((key, value) -> {
                    Boolean isThisCluster = value.getIsThisCluster();
                    Boolean wsClientOnline = value.getWsClientOnline();
                    if (!isThisCluster && wsClientOnline) {
                        //发送用户权限消息
                        String clusterNodeHost = value.getClusterNodeHost();

                        WSMessageRequest wsMessageRequest = null;
                        WSMessageResponse messageResponse = null;
                        try {
                            //TODO 分页查询请求参数
                            UserAuthReq userAuthReq = new UserAuthReq();
                            wsMessageRequest = new WSMessageRequest(Command.USER_AUTH_SYNC, userAuthReq);
                            String targetHostName = FolibWsRunManageUtil.getTargetNode(clusterNodeHost);
                            if (StringUtils.isBlank(targetHostName)) {
                                //WS目标节点未找到，尝试转发到集群中其他节点处理
                                targetHostName = FolibWsRunManageUtil.getTargetHostName(clusterNodeHost);
                                if (folibWsRunManageV2.forward(targetHostName)) {
                                    return ;
                                }
                            }
                            messageResponse = folibWsRunManageV2.sendRequest(targetHostName, wsMessageRequest);
                            if(HttpStatus.OK.equals(messageResponse.getStatus())) {
                                //TODO 添加已更新记录
                            }
                        }  catch (Exception e) {
                            log.error("sendRequest fail,wsMessageRequest:{}", wsMessageRequest, e);
                        }
                    }
                });
                log.info("UserAuthSyncTask thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }
}
