package com.veadan.folib.ws.common;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/13 14:11
 * @since x.x.x
 */
@Slf4j
public abstract class FolibWsRunManage {

    protected static final Map<String, Object> SYNC_ACTION_LOCK_MAP = new ConcurrentHashMap<>();
    protected static final String ACTION_LOCK_MARK = "ACTION_LOCK";

    public static void actionLock(String loackId) {
        SYNC_ACTION_LOCK_MAP.put(loackId, ACTION_LOCK_MARK);
    }

    public static void actionUpdateLockValue(String lockId, Object value) {
        SYNC_ACTION_LOCK_MAP.put(lockId, value);
    }

    public static void actionUnLock(String lockId) {
        SYNC_ACTION_LOCK_MAP.remove(lockId);
    }

    public static <T> T actionUnLockAndGetValue(String lockId, Class<T> valueClass, long timeout, TimeUnit unit) {
        try {
            return CompletableFuture.supplyAsync(() -> {
                Object lockActionValue = SYNC_ACTION_LOCK_MAP.getOrDefault(lockId, ACTION_LOCK_MARK);
                while (lockActionValue.equals(ACTION_LOCK_MARK)) {
                    lockActionValue = SYNC_ACTION_LOCK_MAP.getOrDefault(lockId, ACTION_LOCK_MARK);
                }

                return (T) lockActionValue;
            }).get(timeout, unit);
        } catch (Exception e) {
            log.error("【FolibWs服务端运行管理器】获取同步Action结果失败", e);
        } finally {
            actionUnLock(lockId);
        }

        return null;
    }
}
