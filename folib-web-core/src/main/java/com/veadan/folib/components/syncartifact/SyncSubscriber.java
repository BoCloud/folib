package com.veadan.folib.components.syncartifact;

import com.hazelcast.topic.ITopic;
import com.veadan.folib.components.DistributedTopicComponent;
import com.veadan.folib.services.JfrogMigrateService;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

/**
 * @author huayanjun
 * @since 2024-12-26 17:51
 */
@Component
public class SyncSubscriber {

    @Resource
    private DistributedTopicComponent distributedTopicComponent;

    @Resource
    private JfrogMigrateService jfrogMigrateService;


    @PostConstruct
    public void subscribeQueue() {
        ITopic<String> topic = distributedTopicComponent.getTopic(jfrogMigrateService.TOPIC_QUEUE);
        topic.addMessageListener(message -> {
            jfrogMigrateService.listenTask(message.getMessageObject());
        });
    }

    @PostConstruct
    public void subscribePaused() {
        ITopic<String> topic = distributedTopicComponent.getTopic(jfrogMigrateService.TOPIC_PAUSED);
        topic.addMessageListener(message -> {
            jfrogMigrateService.PAUSED_QUEUE.offer(message.getMessageObject());
        });
    }
}
