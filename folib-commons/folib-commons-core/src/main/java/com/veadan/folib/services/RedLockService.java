package com.veadan.folib.services;

/**
 * @author leipenghui
 **/
public interface RedLockService {

    /**
     * 可重入,必须手动解锁!线程不主动解锁将会永远存在! 慎用
     * 一直尝试(等待)直到获得锁
     *
     * @param lockKey 锁的键
     */
    void lock(String lockKey);

    /**
     * 可重入,必须手动解锁!线程不主动解锁将会永远存在! 慎用
     * 一直尝试(等待)直到获得锁
     *
     * @param lockKey  锁的键
     * @param runnable Run lambda 获得锁后的内部执行业务
     */
    void lock(String lockKey, Runnable runnable);

    /**
     * 可重入,必须手动解锁或等待leaseTime超时
     *
     * @param lockKey   锁的键
     * @param leaseTime 获得锁后的自动解锁时间毫秒(超时时间)
     */
    void lock(String lockKey, long leaseTime);

    /**
     * 支持lambda的加锁模式(自动解锁)
     *
     * @param lockKey   锁的键
     * @param leaseTime 获得锁后的自动解锁时间毫秒(超时时间)
     * @param runnable  Run lambda 获得锁后的内部执行业务
     */
    void lock(String lockKey, long leaseTime, Runnable runnable);

    /**
     * 尝试获取锁
     *
     * @param lockKey  锁的键
     * @param waitTime 尝试获取锁的时间毫秒,为-1时获取不到锁立即返回
     * @return 返回是否获得锁
     * @throws InterruptedException - if the thread is interrupted before or during this method.
     */
    boolean tryLockTimeout(String lockKey, long waitTime) throws InterruptedException;

    /**
     * 尝试获取锁并设置自动解锁时间
     *
     * @param lockKey   锁的键
     * @param waitTime  尝试获取锁的时间毫秒,为-1时获取不到锁立即返回
     * @param leaseTime 获得锁后的自动解锁时间毫秒(超时时间)
     * @return 返回是否获得锁
     * @throws InterruptedException - if the thread is interrupted before or during this method.
     */
    boolean tryLockTimeout(String lockKey, long waitTime, long leaseTime) throws InterruptedException;

    /**
     * 尝试获取锁并设置自动解锁时间
     *
     * @param lockKey   锁的键
     * @param waitTime  尝试获取锁的时间毫秒,为-1时获取不到锁立即返回
     * @param leaseTime 获得锁后的自动解锁时间毫秒(超时时间)
     * @param runnable  Run lambda 获得锁后的内部执行业务
     * @return 返回是否获得锁
     * @throws InterruptedException - if the thread is interrupted before or during this method.
     */
    boolean tryLockTimeout(String lockKey, long waitTime, long leaseTime, Runnable runnable) throws InterruptedException;

    /**
     * 释放锁
     *
     * @param lockKey 锁的键
     */
    void unLock(String lockKey);

}
