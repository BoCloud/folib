package com.veadan.folib.ws.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.web.socket.WebSocketSession;

import javax.websocket.Session;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/8 17:02
 * @since x.x.x
 */
public class FolibWsSessionContextHolder {

    public static final ThreadLocal<Object> sessionContext = new InheritableThreadLocal();

    public static <T extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<?>> void setSession(T session) {
        sessionContext.set(session);
    }

    public static <T extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<?>> T getSession(Class<T> tClass) {
        return (T) sessionContext.get();
    }

    public static void remove() {
        sessionContext.remove();
    }

    @Data
    @Accessors(chain = true)
    @AllArgsConstructor
    @NoArgsConstructor
    public static class FolibWsSessionContextInfo<S> {
        private S session;
    }
}
