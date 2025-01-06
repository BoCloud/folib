package com.veadan.folib.components;

import com.hazelcast.collection.IQueue;
import com.hazelcast.core.HazelcastInstance;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @author huayanjun
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

    public String takeFromQueue(String queueName) throws InterruptedException {
        IQueue<String> queue = hazelcastInstance.getQueue(queueName);
        return queue.take();
    }

}
