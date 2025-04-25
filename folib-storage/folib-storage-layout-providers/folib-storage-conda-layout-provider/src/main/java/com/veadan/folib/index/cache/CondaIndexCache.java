package com.veadan.folib.index.cache;

import com.hazelcast.core.HazelcastInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Repository;

import java.util.Date;

/**
 * @author LingengMa
 * @date 2025/04/22 19:22
 * @Description:
 */


@Component
public class CondaIndexCache {
    private final HazelcastInstance hazelcastInstance;

    @Autowired
    public CondaIndexCache(HazelcastInstance hazelcastInstance) {
        this.hazelcastInstance = hazelcastInstance;
    }

    public boolean isNotModified(String artifactPath, Date ifModifiedSince) {
        if (ifModifiedSince == null) {
            return false;
        }
        if (hazelcastInstance.getMap("condaRepoData").containsKey(artifactPath)) {
            Date lastModified = (Date) hazelcastInstance.getMap("condaRepoData").get(artifactPath);
            return ifModifiedSince.getTime() / 1000 >= lastModified.getTime() / 1000;
        }
        return false;
    }

    public void put(String artifactPath) {
        hazelcastInstance.getMap("condaRepoData").put(artifactPath, new Date());
    }

    public void reset(String artifactPath) {
        hazelcastInstance.getMap("condaRepoData").remove(artifactPath);
    }

    public boolean containsKey(String artifactPath) {
        return hazelcastInstance.getMap("condaRepoData").containsKey(artifactPath);
    }
}
