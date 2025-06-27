package com.veadan.folib.ws.server;

import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.handler.command.FolibWsServerSaveNodeInfoCommand;
import jakarta.websocket.*;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;

import java.nio.ByteBuffer;

/**
 * @author veadan
 * @date 2024/2/12 23:09
 */
@ClientEndpoint
@Slf4j
@Getter
public class FolibWsClient {
    private String uri;
    private String targetHostName;
    private ClusterDispatchNodeDto nodeInfo;

    private ConfigurationManager configurationManager;
    private FolibWsUtil folibWsUtil;
    private FolibWsRunManageV2 folibWsRunManageV2;

    @Override
    public String toString() {
        return "FolibWsClient{" +
                "uri='" + uri + '\'' +
                ", targetHostName='" + targetHostName + '\'' +
                ", nodeInfo=" + nodeInfo +
                '}';
    }

    public FolibWsClient(ClusterDispatchNodeDto nodeInfo, ConfigurationManager configurationManager) {
        this.nodeInfo = nodeInfo;
        this.targetHostName = FolibWsRunManageUtil.getTargetHostName(nodeInfo);
        this.uri = FolibWsRunManageUtil.getWsURI(nodeInfo, configurationManager);
        this.configurationManager = configurationManager;
        this.folibWsUtil = SpringContextUtil.getBean(FolibWsUtil.class);
        this.folibWsRunManageV2 = SpringContextUtil.getBean(FolibWsRunManageV2.class);
    }

    @OnOpen
    public void onOpen(Session session) {
        log.info("OnOpen [{}]", this);
        folibWsUtil.onOpen(targetHostName, session);
        try {
            // 连接到WsServer
            final String clusterNodeHost = nodeInfo.getClusterNodeHost();
            final String baseUrl = configurationManager.getConfiguration().getBaseUrl();
            final String originHost = UrlUtils.getHost(baseUrl);
            final Integer originPort = UrlUtils.getPort(baseUrl);
            final String destHost = UrlUtils.getHost(clusterNodeHost);
            final String destNodeName = String.format("%s", destHost);
            final String originNodeName = String.format("%s:%s", originHost, originPort);


            // 向WsServer发送创建节点维护信息

            final ClusterDispatchNodeDto registerNodeInfoDto = new ClusterDispatchNodeDto();
            BeanUtils.copyProperties(nodeInfo, registerNodeInfoDto);
            registerNodeInfoDto.setAutoRegister(true);
            registerNodeInfoDto.setClusterNodeHost(baseUrl);
            registerNodeInfoDto.setClusterEnName(originNodeName);
            registerNodeInfoDto.setClusterCnName(String.format("【自动注册节点】%s", originNodeName));
            registerNodeInfoDto.setClusterNodeDesc(String.format("【自动注册节点】禁止操作，此节点信息是由客户端节点（%s）向当前节点（%s）发起注册生成", originNodeName, destNodeName));

            FolibWsServerSaveNodeInfoCommand.Payload payload = new FolibWsServerSaveNodeInfoCommand.Payload(registerNodeInfoDto,
                    SyncClusterDispatchEnum.ADD_OR_UPDATE);
            folibWsRunManageV2.sendRequest(targetHostName, new WSMessageRequest(Command.SERVER_INFO, payload));
        } catch (Exception e) {
            log.error("Send server info exception", e);
        }

    }

    @OnClose
    public void onClose(Session session, CloseReason closeReason) {
        folibWsUtil.onClose(targetHostName, session, closeReason);
    }

    @OnMessage
    public void onMessage(ByteBuffer message, Session session) {
        folibWsUtil.onMessage(targetHostName, message, session);
    }

    @OnError
    public void onError(Session session, Throwable error) {
        folibWsUtil.onError(targetHostName, session, error);
    }
}
