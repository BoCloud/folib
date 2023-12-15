package com.veadan.folib.ws.client.init;

import cn.hutool.http.HttpUtil;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.net.URL;
import java.util.Map;

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
        clusterDispatchNode.values().stream()
                // 排除自动注册的节点信息
                .filter(e -> null != e.getAutoRegister() && !e.getAutoRegister())
                .forEach((nodeInfo) -> {
                    final String clusterNodeHost = nodeInfo.getClusterNodeHost();
                    try {
                        final URL destUrl = new URL(clusterNodeHost);
                        final URL originUrl = new URL(configurationManager.getConfiguration().getBaseUrl());
                        final String originHost = originUrl.getHost();
                        final Integer originPort = UrlUtils.getPort(originUrl.toString());
                        final String destHost = destUrl.getHost();
                        final Integer destPort = UrlUtils.getPort(clusterNodeHost);
///                        final String destNodeName = String.format("%s:%s", destHost, destPort);
                        final String originNodeName = String.format("%s:%s", originHost, originPort);
                        final String destUri = String.format("/ws/folib/%s", originNodeName);
                        final boolean enableSSL = HttpUtil.isHttps(clusterNodeHost);
                        
                        FolibWsClientRunManage.up(destHost, destHost, destPort, destUri, true, enableSSL);
                        log.info("【FolibWs连接初始化】开始连接到节点({}:{}) ===> ({}:{})",
                                originHost, originPort,
                                destHost, destPort);
                    } catch (Exception e) {
                        log.error("【FolibWs连接初始化】连接失败", e);
                    }
                });
    }
}
