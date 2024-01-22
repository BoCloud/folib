package com.veadan.folib.components;

import com.hazelcast.core.HazelcastInstance;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author leipenghui
 * @date 2024/1/22
 **/
@Slf4j
@Component
public class IdGenerateUtils {

    @Inject
    private HazelcastInstance hazelcastInstance;

    public long generateId(String key) {
        return hazelcastInstance.getFlakeIdGenerator(key).newId();
    }

}
