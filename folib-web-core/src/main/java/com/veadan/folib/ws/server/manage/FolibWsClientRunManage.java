package com.veadan.folib.ws.server.manage;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import javax.websocket.Session;
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
public class FolibWsClientRunManage
{

    private static final Map<String, FolibWsClientRun> FOLIB_WS_CLIENT_RUN_MAP = new ConcurrentHashMap<>();

    public static boolean online(String nodeName, Session session)
    {
        final FolibWsClientRun folibWsClientRun = FOLIB_WS_CLIENT_RUN_MAP.get(nodeName);
        if (null != folibWsClientRun)
        {
            try {
                folibWsClientRun.getSession().close();
            } catch (IOException e) {
                log.error("【FolibWs客户端上线】，发现关闭已存在会话，进行关闭操作失败", e);
                return false;
            }
        }
        FOLIB_WS_CLIENT_RUN_MAP.put(nodeName, new FolibWsClientRun(nodeName, session, LocalDateTime.now()));

        return true;
    }

    public static boolean offline(String agentId)
    {
        final FolibWsClientRun folibWsClientRun = FOLIB_WS_CLIENT_RUN_MAP.get(agentId);
        if (null != folibWsClientRun)
        {
            try {
                folibWsClientRun.getSession().close();
                FOLIB_WS_CLIENT_RUN_MAP.remove(agentId);
            } catch (IOException e) {
                log.error("【FolibWs客户端下线】，发现关闭存在会话，进行关闭操作失败", e);
                return false;
            }
        }
        else
        { log.error("【FolibWs客户端下线】，未发现关闭存在的连接会话，进行下线操作失败"); }

        return true;
    }

    public static FolibWsClientRun getWsClientRun(String nodeName)
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
    public static class FolibWsClientRun
    {
        @ApiModelProperty(value = "节点名称")
        private String nodeName;
        @ApiModelProperty(value = "Ws客户端会话")
        private Session session;
        @ApiModelProperty(value = "上线时间")
        private LocalDateTime onlineTime;
    }
}
