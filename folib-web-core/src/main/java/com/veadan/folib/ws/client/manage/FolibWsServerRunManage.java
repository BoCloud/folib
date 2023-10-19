package com.veadan.folib.ws.client.manage;

import com.veadan.folib.ws.client.handler.FolibWsClientMessageHandler;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.client.standard.StandardWebSocketClient;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/18 14:05
 * @since x.x.x
 */
@Slf4j
public class FolibWsServerRunManage
{
    private static final Map<String, FolibWsServerRun> FOLIB_WS_CLIENT_RUN_MAP = new ConcurrentHashMap<>();

    public static boolean up(String nodeName, String host, Integer port, String uri, boolean forceUp)
    {
        try {
            final FolibWsServerRun folibWsServerRun = FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
            if (forceUp)
            { remove(nodeName); }
            else
            {
                if (null != folibWsServerRun)
                {
                    log.info("【FolibWs服务端运行管理器】已存在与({}:[{}, {}])节点连接的会话", nodeName, host, port);
                    return false;
                }
            }
            
            final StandardWebSocketClient webSocketClient = new StandardWebSocketClient();
            final String url = String.format("ws://%s:%s%s", host, port, uri);
            final WebSocketSession webSocketSession = webSocketClient.doHandshake(new FolibWsClientMessageHandler(), url).get();
            return online(nodeName, webSocketSession);
        } catch (Exception e) {
            log.error("【FolibWs服务端运行管理器】连接到节点（{}:{}）失败", host, port, e);
            return false;
        }
    }
    
    public static boolean online(String nodeName, WebSocketSession session)
    {
        final FolibWsServerRun folibWsServerRun = FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
        if (null != folibWsServerRun)
        {
            try {
                folibWsServerRun.getSession().close();
            } catch (IOException e) {
                log.error("【FolibWs服务端运行管理器】，发现关闭已存在会话，进行关闭操作失败", e);
                return false;
            }
        }
        FOLIB_WS_CLIENT_RUN_MAP.put(nodeName, new FolibWsServerRun(nodeName, session, LocalDateTime.now()));

        return true;
    }

    public static boolean remove(String nodeName)
    {
        final FolibWsServerRun folibWsServerRun = FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
        if (null != folibWsServerRun)
        {
            try {
                folibWsServerRun.getSession().close();
                FOLIB_WS_CLIENT_RUN_MAP.remove(nodeName);
            } catch (IOException e) {
                log.error("【FolibWs服务端运行管理器】，发现关闭存在会话，进行关闭操作失败", e);
                return false;
            }
        }
        else
        { log.error("【FolibWs服务端运行管理器】，未发现关闭存在的连接会话，进行下线操作失败"); }

        return true;
    }

    public static FolibWsServerRun getWsServerRun(String nodeName)
    {
        return FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
    }
    

    /**
     *
     * @author xiaodong.wang
     * @email wangxiaodong@beyondcent.com
     * @date 2023/10/18 14:07
     * @since x.x.x
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @ApiModel("FolibWs运行管理对象")
    public static class FolibWsServerRun
    {
        @ApiModelProperty(value = "节点名称")
        private String nodeName;
        @ApiModelProperty(value = "Ws服务端会话")
        private WebSocketSession session;
        @ApiModelProperty(value = "上线时间")
        private LocalDateTime onlineTime;
    }
}
