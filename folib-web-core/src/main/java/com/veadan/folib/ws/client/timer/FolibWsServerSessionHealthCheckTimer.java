package com.veadan.folib.ws.client.timer;

import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:23
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsServerSessionHealthCheckTimer {

    @Scheduled(cron = "0/5 * * * * ?")
    public void scan() {
        for (FolibWsClientRunManage.FolibWsServerRun folibWsServerRun : FolibWsClientRunManage.getAllRun()) {
            final String nodeName = folibWsServerRun.getNodeName();
            final String host = folibWsServerRun.getHost();
            final Integer port = folibWsServerRun.getPort();
            final String uri = folibWsServerRun.getUri();
            final boolean isForceUp = folibWsServerRun.isForceUp();
            final boolean enableSSL = folibWsServerRun.isEnableSSL();
            
            if (null == folibWsServerRun.getSession() || !folibWsServerRun.getSessionStatus()) {
                log.info("【Ws连接健康定时任务】扫描到Ws连接（{}）断开，进行重连开始", folibWsServerRun.getWsUrl());
                final boolean reUp = FolibWsClientRunManage.up(nodeName, host, port, uri, isForceUp, enableSSL);
                log.info("【Ws连接健康定时任务】扫描到Ws连接（{}）断开，进行重连结束，重连结果：{}", folibWsServerRun.getWsUrl(), reUp);
            }
        }
    }
}
