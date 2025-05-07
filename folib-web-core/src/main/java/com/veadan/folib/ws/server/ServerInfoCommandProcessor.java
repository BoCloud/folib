package com.veadan.folib.ws.server;

import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.server.handler.command.FolibWsServerSaveNodeInfoCommand;
import jakarta.websocket.Session;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * @author pengYongQiang
 * @date 2024/2/13 16:32
 */
@Component
public class ServerInfoCommandProcessor extends CommandProcessor {
    @Override
    public Command getCommand() {
        return Command.SERVER_INFO;
    }
    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) {
        FolibWsServerSaveNodeInfoCommand.Payload payload = (FolibWsServerSaveNodeInfoCommand.Payload) wsMessageRequest.getDate();
        // 创建分发节点
        final ClusterDispatchNodeDto nodeDto = payload.getNodeDto();
        nodeDto.setCreateTime(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        try {
            ClusterDispatchManagementService bean = SpringContextUtil.getBean(ClusterDispatchManagementService.class);
            bean.createClusterNode(nodeDto);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        // 向其他集群节点同步同步制品分发节点信息
        SpringContextUtil.getBean(ClusterSyncService.class).syncClusterDispatch(payload);
        return "ok";
    }
}
