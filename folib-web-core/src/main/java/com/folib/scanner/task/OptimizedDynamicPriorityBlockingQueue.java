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
package com.folib.scanner.task;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * 优化后的动态优先级阻塞队列
 *
 * @param <E>
 */
public class OptimizedDynamicPriorityBlockingQueue<E> {

    /**
     * 使用PriorityQueue作为内部存储
     */
    private final PriorityQueue<E> queue;

    /**
     * 使用ReentrantLock进行线程同步
     */
    private final ReentrantLock lock;

    /**
     * Condition用于阻塞和唤醒线程
     */
    private final Condition notEmpty;

    /**
     * 当前队列使用的比较器
     */
    private volatile Comparator<? super E> comparator;

    /**
     * 用于存储ID和元素的映射关系
     */
    private final Map<String, E> elementMap;

    /**
     * 构造函数，初始化队列和同步机制
     */
    public OptimizedDynamicPriorityBlockingQueue(Comparator<? super E> comparator) {
        this.comparator = comparator;
        this.queue = new PriorityQueue<>(this.comparator);
        this.lock = new ReentrantLock();
        this.notEmpty = lock.newCondition();
        this.elementMap = new ConcurrentHashMap<>();
    }

    /**
     * 将元素插入队列
     *
     * @param element 元素
     * @param id      元素ID
     */
    public void put(E element, String id) {
        // 加锁
        lock.lock();
        try {
            // 插入元素
            queue.offer(element);
            elementMap.put(id, element);
            // 唤醒一个等待线程
            notEmpty.signal();
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    /**
     * 从队列中取出优先级最高的元素
     *
     * @return 元素
     * @throws InterruptedException 如果线程被中断
     */
    public E take() throws InterruptedException {
        // 加锁
        lock.lock();
        try {
            // 如果队列为空，等待
            while (queue.isEmpty()) {
                // 线程等待
                notEmpty.await();
               //return null;
            }
            // 取出并移除队列头部元素
            E element = queue.poll();
            elementMap.values().remove(element); // 从映射中移除
            return element;
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    public E poll(long timeout, TimeUnit unit) throws InterruptedException {
        long deadline = System.nanoTime() + unit.toNanos(timeout);
        lock.lock();
        try {
            while (queue.isEmpty()) {
                long remaining = deadline - System.nanoTime();
                if (remaining <= 0) {
                    return null; // 超时返回null
                }
                // 释放锁一段时间后再尝试获取
                lock.unlock();
                Thread.sleep(Math.min(TimeUnit.NANOSECONDS.toMillis(remaining), 10)); // 轮询间隔，可以根据需要调整
                lock.lock();
            }
            return queue.poll(); // 取出并移除队列头部元素
        } finally {
            lock.unlock();
        }
    }

    /**
     * 调整队列中某个元素的优先级
     *
     * @param element       元素
     * @param newComparator 新比较器
     */
    public void adjustPriority(E element, Comparator<? super E> newComparator) {
        // 加锁
        lock.lock();
        try {
            if (queue.contains(element)) { // 确保队列中包含该元素
                queue.remove(element); // 从队列中移除元素
                this.comparator = newComparator; // 更新比较器
                queue.offer(element); // 重新插入元素
            }
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    /**
     * 移除元素
     *
     * @param element 元素
     * @return 是否成功移除
     */
    public boolean remove(E element) {
        // 加锁
        lock.lock();
        try {
            // 移除元素
            elementMap.values().remove(element);
            return queue.remove(element);
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    /**
     * 批量插入元素
     *
     * @param elements 元素列表
     * @param ids      元素ID列表
     */
    public void batchPut(Collection<E> elements, Collection<String> ids) {
        // 加锁
        lock.lock();
        try {
            // 插入所有元素
            Iterator<E> elementIterator = elements.iterator();
            Iterator<String> idIterator = ids.iterator();
            while (elementIterator.hasNext() && idIterator.hasNext()) {
                E element = elementIterator.next();
                String id = idIterator.next();
                queue.offer(element);
                elementMap.put(id, element);
            }
            // 唤醒所有等待线程
            notEmpty.signalAll();
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    /**
     * 批量取出元素
     *
     * @param maxElements 最大取出元素数量
     * @return 元素列表
     * @throws InterruptedException 如果线程被中断
     */
    public List<E> batchTake(int maxElements) throws InterruptedException {
        // 加锁
        lock.lock();
        try {
            // 如果队列为空，等待
            while (queue.isEmpty()) {
                // 线程等待
                notEmpty.await();
            }
            // 创建存储取出元素的列表
            List<E> elements = new ArrayList<>(maxElements);
            for (int i = 0; i < maxElements && !queue.isEmpty(); i++) {
                // 取出元素并加入列表
                E element = queue.poll();
                elementMap.values().remove(element);
                elements.add(element);
            }
            // 返回取出的元素列表
            return elements;
        } finally {
            // 释放锁
            lock.unlock();
        }
    }


    /**
     * 获取队列大小
     *
     * @return 队列大小
     */
    public int size() {
        // 加锁
        lock.lock();
        try {
            // 返回队列大小
            return queue.size();
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    /**
     * 检查队列是否为空
     *
     * @return 是否为空
     */
    public boolean isEmpty() {
        // 加锁
        lock.lock();
        try {
            // 返回队列是否为空
            return queue.isEmpty();
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    /**
     * 查看队列中的所有元素（不移除）
     *
     * @return 元素列表
     */
    public List<E> peekAll() {
        // 加锁
        lock.lock();
        try {
            // 返回队列中所有元素的副本
            return new ArrayList<>(queue);
        } finally {
            // 释放锁
            lock.unlock();
        }
    }

    /**
     * 根据ID获取元素
     *
     * @param id 元素ID
     * @return 元素
     */
    public E getElementById(String id) {
        lock.lock();
        try {
            return elementMap.get(id);
        } finally {
            lock.unlock();
        }
    }
}
