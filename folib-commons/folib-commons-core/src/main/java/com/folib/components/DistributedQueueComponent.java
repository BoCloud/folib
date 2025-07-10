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
package com.folib.components;

import com.hazelcast.collection.IQueue;
import com.hazelcast.core.HazelcastInstance;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.concurrent.TimeUnit;

/**
 * @author veadan
 * @since 2024-12-25 17:40
 */
@Slf4j
@Component
public class DistributedQueueComponent {

    @Resource
    private HazelcastInstance hazelcastInstance;


    public void putToQueue(String queueName, String message) throws InterruptedException {
        IQueue<String> queue = hazelcastInstance.getQueue(queueName);
        queue.put(message);
    }

    // 会一直阻塞
    public String takeFromQueue(String queueName) throws InterruptedException {
        IQueue<String> queue = hazelcastInstance.getQueue(queueName);
        return queue.take();
    }

    // 超过指定时间，队列无数据会返回null
    public String pollFromQueue(String queueName, long time,TimeUnit unit) throws InterruptedException {
        IQueue<String> queue = hazelcastInstance.getQueue(queueName);
        return queue.poll(time,unit);
    }

}
