package com.folib.components;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.topic.ITopic;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @author veadan
 * @since 2024-12-26 17:20
 */

@Slf4j
@Component
public class DistributedTopicComponent {

    @Resource
    private HazelcastInstance hazelcastInstance;

    public void publishMessage(String topicName, String message) {
        ITopic<String> topic = hazelcastInstance.getTopic(topicName);
        topic.publish(message);
    }

    public ITopic<String> getTopic(String topicName) {
        return hazelcastInstance.getTopic(topicName);
    }


}
