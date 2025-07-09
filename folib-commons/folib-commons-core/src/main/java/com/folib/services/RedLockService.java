/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.services;

/**
 * @author veadan
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
