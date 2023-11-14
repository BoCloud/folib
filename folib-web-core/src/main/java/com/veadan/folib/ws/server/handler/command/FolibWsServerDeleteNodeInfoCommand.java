package com.veadan.folib.ws.server.handler.command;

import com.veadan.folib.controllers.cluster.dto.SyncClusterDispatchDto;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ClusterSyncService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/8 14:26
 * @since x.x.x
 */
@Component
@Slf4j
public class FolibWsServerDeleteNodeInfoCommand implements FolibWsServerCommand<SyncClusterDispatchDto> {

    public static final String COMMAND = "/server/deleteNodeInfo";


    @Autowired
    private ClusterDispatchManagementService clusterDispatchManagementService;
    @Autowired
    private ClusterSyncService clusterSyncService;
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(SyncClusterDispatchDto dispatchDto) {
        final String clusterEnName = dispatchDto.getNodeDto().getClusterEnName();
        try {
            clusterDispatchManagementService.deleteClusterNode(dispatchDto.getNodeDto());
            // 向其他集群节点同步制品分发节点信息
            clusterSyncService.syncClusterDispatch(dispatchDto);
            log.error("删除节点（{}）成功", clusterEnName);
        } catch (Exception e) {
            log.error("删除节点（{}）失败", clusterEnName, e);
        }
    }
    
}
