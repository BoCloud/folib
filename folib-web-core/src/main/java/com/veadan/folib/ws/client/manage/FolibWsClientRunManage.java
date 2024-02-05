package com.veadan.folib.ws.client.manage;

import cn.hutool.core.util.StrUtil;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.ws.client.handler.FolibWsClientMessageHandler;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsRunManage;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.PingMessage;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.client.jetty.JettyWebSocketClient;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Collection;
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
public class FolibWsClientRunManage extends FolibWsRunManage {
    private static final Map<String, FolibWsServerRun> FOLIB_WS_RUN_MAP = new ConcurrentHashMap<>();
    public static final JettyWebSocketClient WEB_SOCKET_CLIENT = new JettyWebSocketClient();

    static {
        WEB_SOCKET_CLIENT.start();
    }

    public static Collection<FolibWsServerRun> getAllRun() {
        return FOLIB_WS_RUN_MAP.values();
    }

    public static boolean up(String nodeName, String host, Integer port, String uri, boolean forceUp) {
        return up(nodeName, host, port, uri, forceUp, false);
    }

    public static boolean up(String nodeName, String host, Integer port, String uri, boolean forceUp, boolean enableSSL) {
        try {
            FolibWsServerRun folibWsServerRun = FOLIB_WS_RUN_MAP.get(nodeName);
            if (forceUp) {
                down(nodeName);
            } else {
                if (null != folibWsServerRun) {
                    log.info("【FolibWs服务端运行管理器-启动】已存在与({}:[{}, {}])节点连接的会话", nodeName, host, port);
                    return false;
                }
            }

//            final StandardWebSocketClient socketClient = new StandardWebSocketClient();
            if (null == folibWsServerRun) {
                folibWsServerRun = new FolibWsServerRun();
            }

            folibWsServerRun.setNodeName(nodeName);
            folibWsServerRun.setHost(host);
            folibWsServerRun.setPort(port);
            folibWsServerRun.setUri(uri);
            folibWsServerRun.setForceUp(forceUp);
            folibWsServerRun.setEnableSSL(enableSSL);
            FOLIB_WS_RUN_MAP.put(nodeName, folibWsServerRun);
            final String url = folibWsServerRun.getWsUrl();
            final WebSocketSession webSocketSession = WEB_SOCKET_CLIENT.doHandshake(new FolibWsClientMessageHandler(), url).get();
            log.info("【FolibWs服务端运行管理器-启动】连接到节点（{}:{}）成功", host, port);
            folibWsServerRun.setSession(webSocketSession);
            folibWsServerRun.setOnlineTime(LocalDateTime.now());
            webSocketSession.sendMessage(new PingMessage());
//            FOLIB_WS_RUN_MAP.put(nodeName, folibWsServerRun);

            return true;
        } catch (Exception e) {
            log.debug("【FolibWs服务端运行管理器-启动】连接到节点（{}:{}）失败 {}", host, port, ExceptionUtils.getStackTrace(e));
            return false;
        }
    }

    public static boolean down(String nodeName) {
        final FolibWsServerRun folibWsServerRun = FOLIB_WS_RUN_MAP.get(nodeName);
        if (null != folibWsServerRun) {
            try {
                if (null != folibWsServerRun.getSession() && folibWsServerRun.getSession().isOpen()) {
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
        final FolibWsServerRun folibWsServerRun = FOLIB_WS_RUN_MAP.get(nodeName);
        if (null != folibWsServerRun) {
            try {
                if (null != folibWsServerRun.getSession() && folibWsServerRun.getSession().isOpen()) {
                    folibWsServerRun.getSession().close();
                }
                FOLIB_WS_RUN_MAP.remove(nodeName);
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
        return FOLIB_WS_RUN_MAP.get(nodeName);
    }

    public static FolibWsServerRun findRunBySession(WebSocketSession session) {
        return FOLIB_WS_RUN_MAP.values()
                .stream()
                .filter(e -> null != e.getSession() && e.getSession().equals(session))
                .findFirst()
                .orElse(null);
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
        @ApiModelProperty(value = "启用SSL")
        private boolean enableSSL;
        @ApiModelProperty(value = "Ws服务端会话")
        private WebSocketSession session;
        @ApiModelProperty(value = "上线时间")
        private LocalDateTime onlineTime;

        public String getWsUrl() {
            return String.format("ws%s://%s:%s%s", (this.enableSSL ? "s" : StrUtil.EMPTY), this.host, this.port, this.uri);
        }

        public boolean getSessionStatus() {
            return null != this.session && this.session.isOpen();
        }

        public boolean ping() {
            try {
                if (this.getSessionStatus()) {
                    this.session.sendMessage(new PingMessage());
                    return true;
                }
                return false;
            } catch (Exception e) {
                return false;
            }
        }

        public boolean doAction(FolibWsAction folibWsAction) {
            try {
                if (null == this.session) {
                    throw new BusinessException("发起请求失败，还未创建Ws会话");
                }
                if (!this.session.isOpen()) {
                    throw new BusinessException("发起请求失败，Ws会话已经关闭");
                }

                // 发起请求
                this.session.sendMessage(new TextMessage(folibWsAction.encode()));
                return true;
            } catch (Exception e) {
                log.error("发起Ws异步请求失败", e);
                return false;
            }
        }

        public <T> T doSyncAction(FolibWsAction folibWsAction, Class<T> responseClass) {
            final String syncId = folibWsAction.sync().getSyncId();
            try {
                if (null == this.session) {
                    throw new BusinessException("发起请求失败，还未创建Ws会话");
                }
                if (!this.session.isOpen()) {
                    throw new BusinessException("发起请求失败，Ws会话已经关闭");
                }

                actionLock(syncId);
                // 发起请求
                this.session.sendMessage(new TextMessage(folibWsAction.encode()));
                return actionUnLockAndGetValue(syncId, responseClass, 10, TimeUnit.SECONDS);
            } catch (Exception e) {
                log.error("发起Ws同步请求失败", e);
                return null;
            } finally {
                actionUnLock(syncId);
            }
        }
    }
}
