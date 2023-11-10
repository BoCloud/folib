package com.veadan.folib.ws.server.handler.command;

import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.controllers.cluster.dto.SyncClusterDispatchDto;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.common.JsonEncoder;
import com.veadan.folib.ws.server.context.FolibWsServerContextInfo;
import com.veadan.folib.ws.server.manage.FolibWsClientRunManage;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/8 14:26
 * @since x.x.x
 */
@Component
@Slf4j
public class FolibWsServerSaveNodeInfoCommand implements FolibWsServerCommand<FolibWsServerSaveNodeInfoCommand.Payload> {

    public static final String COMMAND = "/saveNodeInfo";


    @Autowired
    private ClusterDispatchManagementService clusterDispatchManagementService;
    @Autowired
    private ClusterSyncService clusterSyncService;
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(Payload payload) {
        try {
            // 创建分发节点
            final ClusterDispatchNodeDto nodeDto = payload.getNodeDto();
            nodeDto.setCreateTime(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
            clusterDispatchManagementService.createClusterNode(nodeDto);

            // 向其他集群节点同步同步制品分发节点信息
            clusterSyncService.syncClusterDispatch(payload);
            
            // 断开与WsClient的连接
            final FolibWsServerContextInfo session = FolibWsSessionContextHolder.getContextSessionInfo(FolibWsServerContextInfo.class);
            FolibWsClientRunManage.remove(session.getNodeName());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    @Data
    @EqualsAndHashCode(callSuper = true)
    public static class Payload extends SyncClusterDispatchDto implements JsonEncoder {
        public Payload(ClusterDispatchNodeDto nodeDto, SyncClusterDispatchEnum syncClusterDispatchEnum) {
            super(nodeDto, syncClusterDispatchEnum);
        }
    }
}
