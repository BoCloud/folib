package com.folib.cache;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.folib.storage.repository.Repository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;



@Component
public class NugetRemoteUrlCache {
    private final IMap<String, String> map;

    @Autowired
    public NugetRemoteUrlCache(@Qualifier("hazelcastInstance") HazelcastInstance hazelcastInstance) {
        String REGISTRATION_ITEM_CACHE_NAME = "nugetRemoteRegistrationItemCache";
        this.map = hazelcastInstance.getMap(REGISTRATION_ITEM_CACHE_NAME);
    }

    // registration-item
    public void putItem(Repository repository, String packageId, String version, String url) {
        String key = getItemKey(repository, packageId, version);
        if (map.containsKey(key)) {
            return;
        }
        map.put(key, url);
    }

    public String getItem(Repository repository, String packageId, String version) {
        String key = getItemKey(repository, packageId, version);
        return map.get(key);
    }

    private String getItemKey(Repository repository, String packageId, String version) {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String prefix = "registration-item";
        packageId = packageId.toLowerCase();
        return String.format("%s:%s:%s:%s:%s", prefix, storageId, repositoryId, packageId, version);
    }


    // registration-page
    public void putPage(Repository repository, String packageId, String lower, String upper, String url, boolean isSemVer2Endpoint) {
        String key = getPageKey(repository, packageId, lower, upper, isSemVer2Endpoint);
        if (map.containsKey(key)) {
            return;
        }
        String commonKey = getPageKey(repository, packageId, "@@", "^^", isSemVer2Endpoint);
        if (!map.containsKey(commonKey) && url.contains(lower) && url.contains(upper)) {
            String new_url = url;
            // 将第一个lower替换为@@
            new_url = new_url.replaceFirst(lower, "@@")
                             .replaceFirst(upper, "^^");
            map.put(commonKey, new_url);
        }
        map.put(key, url);
    }

    public String getPage(Repository repository, String packageId, String lower, String upper, boolean isSemVer2Endpoint) {
        String key = getPageKey(repository, packageId, lower, upper, isSemVer2Endpoint);
        return map.get(key);
    }

    private String getPageKey(Repository repository, String packageId, String lower, String upper, boolean isSemVer2Endpoint) {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String prefix = isSemVer2Endpoint ? "registration-semver2" : "registration";
        packageId = packageId.toLowerCase();
        return String.format("%s:%s:%s:%s:%s:%s", prefix, storageId, repositoryId, packageId, lower, upper);
    }


    // content
    public void putContent(Repository repository, String packageId, String version, String url) {
        String key = getContentKey(repository, packageId, version);
        if (map.containsKey(key)) {
            return;
        }
        map.put(key, url);
    }

    public String getContent(Repository repository, String packageId, String version) {
        String key = getContentKey(repository, packageId, version);
        return map.get(key);
    }

    private String getContentKey(Repository repository, String packageId, String version) {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        packageId = packageId.toLowerCase();
        String prefix = "content";
        return String.format("%s:%s:%s:%s:%s", prefix, storageId, repositoryId, packageId, version);
    }

    // registration
    public void putRegistration(Repository repository, String packageId, String url) {
        String key = getRegistrationKey(repository, packageId);
        if (map.containsKey(key)) {
            return;
        }
        map.put(key, url);
    }

    public String getRegistration(Repository repository, String packageId) {
        String key = getRegistrationKey(repository, packageId);
        return map.get(key);
    }

    private String getRegistrationKey(Repository repository, String packageId) {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String prefix = "registration";
        packageId = packageId.toLowerCase();
        return String.format("%s:%s:%s:%s", prefix, storageId, repositoryId, packageId);
    }

}
