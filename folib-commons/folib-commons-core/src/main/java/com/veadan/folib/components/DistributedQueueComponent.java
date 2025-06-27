package com.veadan.folib.components;

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
