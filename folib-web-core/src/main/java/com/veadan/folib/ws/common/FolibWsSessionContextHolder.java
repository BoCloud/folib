package com.veadan.folib.ws.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author veadan
 * @date 2023/11/8 17:02
 */
public class FolibWsSessionContextHolder {

    public static final ThreadLocal<Object> sessionContext = new InheritableThreadLocal();

    public static <T extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<?>> void setContextSessionInfo(T session) {
        sessionContext.set(session);
    }

    public static <T extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<?>> T getContextSessionInfo(Class<T> tClass) {
        return (T) sessionContext.get();
    }

    public static void removeContextSessionInfo() {
        sessionContext.remove();
    }

    @Data
    @Accessors(chain = true)
    @AllArgsConstructor
    @NoArgsConstructor
    public static class FolibWsSessionContextInfo<R> {
        private R wsRunInfo;
        private String syncId;
    }
}
