package com.veadan.folib.ws.client.init;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.client.manage.FolibWsServerRunManage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.net.URL;
import java.util.Map;
import java.util.Optional;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:25
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsServerConnectionInit implements ApplicationRunner 
{
    @Inject
    protected ConfigurationManager configurationManager;
    @Autowired
    private ConfigurationManagementService configurationManagementService;
    
    @Override
    public void run(ApplicationArguments args) throws Exception 
    {
        // 初始化连接到集群服务端
        final Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
        clusterDispatchNode.values().forEach((nodeInfo) -> {
            final String clusterNodeHost = nodeInfo.getClusterNodeHost();
            try {
                final URL destUrl = new URL(clusterNodeHost);
                final URL originUrl = new URL(configurationManager.getConfiguration().getBaseUrl());
                final String destHost = destUrl.getHost();
                final Integer destPort = UrlUtils.getPort(clusterNodeHost);
                final String nodeName = String.format("%s:%s", destHost, destPort);
                final String destUri = String.format("/ws/folib/%s", nodeName);
                
                FolibWsServerRunManage.up(nodeName, destHost, destPort, destUri, false);
                log.info("【FolibWs连接初始化】开始连接到节点({}:{}) ===> ({}:{})", 
                        originUrl.getHost(), originUrl.getPort(),
                        destHost, destPort);
            } catch (Exception e) {
                log.error("【FolibWs连接初始化】连接失败", e);
            }
        });
    }
}
