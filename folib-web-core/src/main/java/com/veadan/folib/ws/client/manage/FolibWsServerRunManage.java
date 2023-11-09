package com.veadan.folib.ws.client.manage;

import cn.hutool.core.date.DateUtil;
import com.veadan.folib.ws.client.handler.FolibWsClientMessageHandler;
import com.veadan.folib.ws.client.handler.command.FolibWsClientConsoleCommand;
import com.veadan.folib.ws.common.FolibWsAction;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.client.standard.StandardWebSocketClient;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/18 14:05
 * @since x.x.x
 */
@Slf4j
public class FolibWsServerRunManage {
    private static final Map<String, FolibWsServerRun> FOLIB_WS_CLIENT_RUN_MAP = new ConcurrentHashMap<>();

    public static Collection<FolibWsServerRun> getAllRun() {
        return FOLIB_WS_CLIENT_RUN_MAP.values();
    } 
    
    public static boolean up(String nodeName, String host, Integer port, String uri, boolean forceUp) {
        try {
            FolibWsServerRun folibWsServerRun = FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
            if (forceUp) {
                down(nodeName);
            } else {
                if (null != folibWsServerRun) {
                    log.info("【FolibWs服务端运行管理器-启动】已存在与({}:[{}, {}])节点连接的会话", nodeName, host, port);
                    return false;
                }
            }

//            final JettyWebSocketClient socketClient = new JettyWebSocketClient();
//            socketClient.start();
            final StandardWebSocketClient socketClient = new StandardWebSocketClient();

            if (null == folibWsServerRun) {
                folibWsServerRun = new FolibWsServerRun();
            }
            folibWsServerRun.setNodeName(nodeName);
            folibWsServerRun.setHost(host);
            folibWsServerRun.setPort(port);
            folibWsServerRun.setUri(uri);
            folibWsServerRun.setForceUp(forceUp);
            FOLIB_WS_CLIENT_RUN_MAP.put(nodeName, folibWsServerRun);
            final String url = folibWsServerRun.getWsUrl();
            final WebSocketSession webSocketSession = socketClient.doHandshake(new FolibWsClientMessageHandler(), url).get();
//            final WebSocketSession webSocketSession = webSocketClient.doHandshake(new FolibWsClientMessageHandler(), url).get();
            log.info("【FolibWs服务端运行管理器-启动】连接到节点（{}:{}）成功", host, port);
            folibWsServerRun.setSession(webSocketSession);
            folibWsServerRun.setOnlineTime(LocalDateTime.now());
            
            return true;
        } catch (Exception e) {
            log.error("【FolibWs服务端运行管理器-启动】连接到节点（{}:{}）失败", host, port, e);
            return false;
        }
    }

    public static boolean down(String nodeName) {
        final FolibWsServerRun folibWsServerRun = FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
        if (null != folibWsServerRun) {
            try {
                if (folibWsServerRun.getSession().isOpen()) {
                    folibWsServerRun.getSession().close();
                }
                log.error("【FolibWs服务端运行管理器-停止】停止会话成功");
            } catch (IOException e) {
                log.error("【FolibWs服务端运行管理器-停止】发现关闭存在会话，进行关闭操作失败", e);
                return false;
            }
        } else {
            log.error("【FolibWs服务端运行管理器-停止】，未发现关闭存在的连接会话，进行下线操作失败");
        }
        
        return true;
    }

    public static boolean remove(String nodeName) {
        final FolibWsServerRun folibWsServerRun = FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
        if (null != folibWsServerRun) {
            try {
                if (folibWsServerRun.getSession().isOpen()) {
                    folibWsServerRun.getSession().close();
                }
                FOLIB_WS_CLIENT_RUN_MAP.remove(nodeName);
                log.error("【FolibWs服务端运行管理器】移除会话成功");
            } catch (IOException e) {
                log.error("【FolibWs服务端运行管理器】发现关闭存在会话，进行关闭操作失败", e);
                return false;
            }
        } else {
            log.error("【FolibWs服务端运行管理器】，未发现关闭存在的连接会话，进行下线操作失败");
        }

        return true;
    }

    public static FolibWsServerRun getWsServerRun(String nodeName) {
        return FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
    }


    /**
     * @author xiaodong.wang
     * @email wangxiaodong@beyondcent.com
     * @date 2023/10/18 14:07
     * @since x.x.x
     */
    @Data
    @Accessors(chain = true)
    @AllArgsConstructor
    @NoArgsConstructor
    @ApiModel("FolibWs运行管理对象")
    public static class FolibWsServerRun {
        @ApiModelProperty(value = "节点名称")
        private String nodeName;
        @ApiModelProperty(value = "主机地址")
        private String host;
        @ApiModelProperty(value = "端口")
        private Integer port; 
        @ApiModelProperty(value = "uri")
        private String uri;
        @ApiModelProperty(value = "强制启动")
        private boolean forceUp;
        @ApiModelProperty(value = "Ws服务端会话")
        private WebSocketSession session;
        @ApiModelProperty(value = "上线时间")
        private LocalDateTime onlineTime;
        
        public String getWsUrl() {
            return String.format("ws://%s:%s%s", this.host, this.port, this.uri);
        }
    }

    public static void main(String[] args) throws Exception {
        final String nnodeName = "zhangsan";
        FolibWsServerRunManage.up(nnodeName, "10.50.8.55", 38080, "/ws/folib/zhangsan", true);
        final FolibWsServerRun wsServerRun = FolibWsServerRunManage.getWsServerRun(nnodeName);
        for (int i = 0; i < 10; i++) {
            TimeUnit.SECONDS.sleep(1L);
            log.info("发送消息：{}", i);

            wsServerRun.getSession().sendMessage(new TextMessage(
                    new FolibWsAction().setCommand(FolibWsClientConsoleCommand.COMMAND).setPayload(new FolibWsClientConsoleCommand.Payload()
                            .setLevel(FolibWsClientConsoleCommand.LogConsoleLevel.INFO)
                            .setContent(String.format("当前时间：%s", DateUtil.format(new Date(), "yyyy-MM-dd HH:mm:ss"))).encode()
                    ).encode()
            ));
        }
        new Thread(() -> {
            final FolibWsServerRun wsServerRun1 = FolibWsServerRunManage.getWsServerRun(nnodeName);
            try {
                log.info("另一个线程中发送消息");
                wsServerRun1.getSession().sendMessage(new TextMessage(
                        new FolibWsAction().setCommand(FolibWsClientConsoleCommand.COMMAND).setPayload(new FolibWsClientConsoleCommand.Payload()
                                .setLevel(FolibWsClientConsoleCommand.LogConsoleLevel.INFO)
                                .setContent(String.format("在另一个线程中，当前时间：%s", DateUtil.format(new Date(), "yyyy-MM-dd HH:mm:ss"))).encode()
                        ).encode()
                ));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }, "CCCCCC-11").start();
    }
}
