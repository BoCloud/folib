package com.veadan.folib.components;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.cp.IAtomicLong;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @author huayanjun
 * @since 2024-12-27 13:39
 */
@Slf4j
@Component
public class DistributedCounterComponent {

    @Resource
    private HazelcastInstance hazelcastInstance;

    public IAtomicLong getAtomicLong(String name){
        return hazelcastInstance.getCPSubsystem().getAtomicLong(name);
    }


}
