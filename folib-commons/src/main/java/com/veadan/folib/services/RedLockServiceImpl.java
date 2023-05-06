package com.veadan.folib.services;

import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class RedLockServiceImpl implements RedLockService {

    @Autowired(required = false)
    private RedissonClient redissonClient;

    @Override
    public void lock(String lockKey) {
        RLock lock1 = redissonClient.getLock(lockKey);
        redissonClient.getRedLock(lock1).lock();
    }

    @Override
    public void lock(String lockKey, Runnable runnable) {
        lock(lockKey);
        try {
            runnable.run();
        } finally {
            this.unLock(lockKey);
        }
    }

    @Override
    public void lock(String lockKey, long leaseTime) {
        RLock lock1 = redissonClient.getLock(lockKey);
        redissonClient.getRedLock(lock1).lock(leaseTime, TimeUnit.MILLISECONDS);
    }

    @Override
    public void lock(String lockKey, long leaseTime, Runnable runnable) {
        lock(lockKey, leaseTime);
        try {
            runnable.run();
        } finally {
            this.unLock(lockKey);
        }
    }

    @Override
    public boolean tryLockTimeout(String lockKey, long waitTime) throws InterruptedException {
        RLock lock1 = redissonClient.getLock(lockKey);
        return redissonClient.getRedLock(lock1).tryLock(waitTime, TimeUnit.MILLISECONDS);
    }

    @Override
    public boolean tryLockTimeout(String lockKey, long waitTime, long leaseTime) throws InterruptedException {
        RLock lock1 = redissonClient.getLock(lockKey);
        return redissonClient.getRedLock(lock1).tryLock(waitTime, leaseTime, TimeUnit.MILLISECONDS);
    }

    @Override
    public boolean tryLockTimeout(String lockKey, long waitTime, long leaseTime, Runnable runnable) throws InterruptedException {
        if (tryLockTimeout(lockKey, waitTime, leaseTime)) {
            try {
                runnable.run();
            } finally {
                this.unLock(lockKey);
            }
            return true;
        }
        return false;
    }

    @Override
    public void unLock(String lockKey) {
        RLock lock = redissonClient.getLock(lockKey);
        if (Objects.nonNull(lock) && lock.isLocked() && lock.isHeldByCurrentThread()) {
            log.info("Current ThreadName：{} lockKey：{} unlock", Thread.currentThread().getName(), lockKey);
            lock.unlock();
        }
    }
}
