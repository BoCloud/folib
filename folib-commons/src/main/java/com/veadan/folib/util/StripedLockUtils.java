package com.veadan.folib.util;

import com.google.common.util.concurrent.Striped;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.locks.Lock;

/**
 * @author leipenghui
 * @date 2023/3/1
 **/
@Slf4j
public class StripedLockUtils {

    private static Striped<Lock> locks = Striped.lazyWeakLock(1024);

    public static Lock lock(String lockKey) {
        return locks.get(lockKey);
    }

}
