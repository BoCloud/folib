package com.veadan.folib.ws.server.manage;

import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsRunManage;
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
import java.util.concurrent.TimeUnit;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/18 14:05
 * @since x.x.x
 */
@Slf4j
public class FolibWsServerRunManage extends FolibWsRunManage {

    private static final Map<String, FolibWsClientRun> FOLIB_WS_RUN_MAP = new ConcurrentHashMap<>();

    public static boolean online(String nodeName, Session session) {
        final FolibWsClientRun folibWsClientRun = FOLIB_WS_RUN_MAP.get(nodeName);
        if (null != folibWsClientRun) {
            try {
                folibWsClientRun.getSession().close();
            } catch (IOException e) {
                log.error("【FolibWs客户端上线】，发现关闭已存在会话，进行关闭操作失败", e);
                return false;
            }
        }
        FOLIB_WS_RUN_MAP.put(nodeName, new FolibWsClientRun(nodeName, session, LocalDateTime.now()));

        return true;
    }

    public static boolean remove(String nodeName) {
        final FolibWsClientRun folibWsClientRun = FOLIB_WS_RUN_MAP.get(nodeName);
        if (null != folibWsClientRun) {
            try {
                folibWsClientRun.getSession().close();
                FOLIB_WS_RUN_MAP.remove(nodeName);
            } catch (IOException e) {
                log.error("【FolibWs客户端下线】，发现关闭存在会话，进行关闭操作失败", e);
                return false;
            }
        } else {
            log.error("【FolibWs客户端下线】，未发现关闭存在的连接会话，进行下线操作失败");
        }

        return true;
    }

    public static FolibWsClientRun getWsClientRun(String nodeName) {
        return FOLIB_WS_RUN_MAP.get(nodeName);
    }

    public static FolibWsClientRun findRunBySession(Session session) {
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
    @AllArgsConstructor
    @NoArgsConstructor
    @ApiModel("FolibWs运行管理对象")
    public static class FolibWsClientRun {
        @ApiModelProperty(value = "节点名称")
        private String nodeName;
        @ApiModelProperty(value = "Ws客户端会话")
        private Session session;
        @ApiModelProperty(value = "上线时间")
        private LocalDateTime onlineTime;

        public boolean getSessionStatus() {
            return null != this.session && this.session.isOpen();
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
                this.session.getBasicRemote().sendText(folibWsAction.encode());
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
                this.session.getBasicRemote().sendText(folibWsAction.encode());
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
