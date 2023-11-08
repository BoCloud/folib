package com.veadan.folib.ws.server.handler.command;

import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.controllers.cluster.dto.SyncClusterDispatchDto;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.ws.common.JsonEncoder;
import lombok.Data;
import lombok.EqualsAndHashCode;
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
public class FolibWsServerDeleteNodeInfoCommand implements FolibWsServerCommand<String> {

    public static final String COMMAND = "/deleteNodeInfo";


    @Autowired
    private ClusterDispatchManagementService clusterDispatchManagementService;
    @Autowired
    private ClusterSyncService clusterSyncService;
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(String clusterEnName) {
        try {
            final ClusterDispatchNodeDto nodeDto = new ClusterDispatchNodeDto();
            nodeDto.setClusterEnName(clusterEnName);
            clusterDispatchManagementService.deleteClusterNode(nodeDto);

            // 向其他集群节点同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.DELETE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);
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
