package com.veadan.folib.ws.server.handler;

import com.veadan.folib.ws.server.manage.FolibWsClientManage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.util.CollectionUtils;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.util.List;

/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/18 14:17
 * @since x.x.x
 */
@Slf4j
public class FolibWsServerHandler extends TextWebSocketHandler
{
    /** Folib节点名称-Header头字段名称*/
    private static final String FOLIB_NODE_NAME_HEADER_FIELD_NAME = "Folib-Node-Name";
    
    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception 
    {
        final String nodeName = this.getNodeName(session);
        log.info("【FolibWs客户端处理器】（{}）上线", nodeName);
        FolibWsClientManage.online(nodeName, session);
    }

    @Override
    public void handleTransportError(WebSocketSession session, Throwable throwable) throws Exception {
        log.error("【FolibWs客户端处理器】请求错误: " + session.getId());
        throwable.printStackTrace();
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus closeStatus) throws Exception {
        final String nodeName = this.getNodeName(session);
        log.info("【FolibWs客户端处理器】（{}）下线", nodeName);
        FolibWsClientManage.offline(nodeName);
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception 
    {
        // TODO: 
    }


    private String getNodeName(WebSocketSession session)
    {
        final HttpHeaders handshakeHeaders = session.getHandshakeHeaders();
        final List<String> agentIds = handshakeHeaders.get(FOLIB_NODE_NAME_HEADER_FIELD_NAME);
        if (!CollectionUtils.isEmpty(agentIds))
        { return agentIds.get(0); }

        return null;
    }
}
