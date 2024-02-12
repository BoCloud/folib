package com.veadan.folib.ws.client.timer;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:23
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsServerSessionHealthCheckTimer {
    @Inject
    protected ConfigurationManager configurationManager;
    @Autowired
    private ConfigurationManagementService configurationManagementService;
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;

    @Scheduled(cron = "0/5 * * * * ?")
    public void scan() {
        for (FolibWsClientRunManage.FolibWsServerRun folibWsServerRun : FolibWsClientRunManage.getAllRun()) {
            final String nodeName = folibWsServerRun.getNodeName();
            final String host = folibWsServerRun.getHost();
            final Integer port = folibWsServerRun.getPort();
            final String uri = folibWsServerRun.getUri();
            final boolean isForceUp = folibWsServerRun.isForceUp();
            final boolean enableSSL = folibWsServerRun.isEnableSSL();

            // 保持心跳，避免nginx反向代理配置断开
            if (!folibWsServerRun.ping()) {
                log.debug("【Ws连接健康定时任务】扫描到Ws连接（{}）断开，进行重连开始", folibWsServerRun.getWsUrl());
                final boolean reUp = FolibWsClientRunManage.up(nodeName, host, port, uri, isForceUp, enableSSL);
                log.debug("【Ws连接健康定时任务】扫描到Ws连接（{}）断开，进行重连结束，重连结果：{}", folibWsServerRun.getWsUrl(), reUp);
            }
        }
    }
}
