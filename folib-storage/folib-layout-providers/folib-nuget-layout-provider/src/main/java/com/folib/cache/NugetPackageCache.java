package com.folib.cache;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.folib.nuget.odata.feed.Entry;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.storage.repository.Repository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


@Component
public class NugetPackageCache {
    private final IMap<String, Map<String, RegistrationResultPageItem>> mapV3;
    private final IMap<String, Map<String, Entry>> mapV2;


    @Autowired
    public NugetPackageCache(@Qualifier("hazelcastInstance") HazelcastInstance hazelcastInstance) {
        String REGISTRATION_CACHE_NAME = "nugetRegistrationResultPageItem";
        String FEED_PROPERTIES_CACHE_NAME = "nugetFeed";
        this.mapV3 = hazelcastInstance.getMap(REGISTRATION_CACHE_NAME);
        this.mapV2 = hazelcastInstance.getMap(FEED_PROPERTIES_CACHE_NAME);
    }


    public void put(Repository repository, String lower_id, String version, RegistrationResultPageItem registrationResultPageItem, Entry entry) {
        String key = getKey(repository,lower_id);
        // registration
        if (!mapV3.containsKey(key)) {
            mapV3.put(key, new HashMap<>());
        }
        Map<String, RegistrationResultPageItem> versionMap = mapV3.get(key);
        versionMap.put(version, registrationResultPageItem);
        mapV3.put(key, versionMap);

        // feed
        if (!mapV2.containsKey(key)) {
            mapV2.put(key, new HashMap<>());
        }
        Map<String, Entry> propertiesMap = mapV2.get(key);
        propertiesMap.put(version, entry);
        mapV2.put(key, propertiesMap);
    }

    /**
     * @param repository
     * @return <packageId, <version, RegistrationResultPageItem>>
     */
    public Map<String, Map<String, RegistrationResultPageItem>> getAllRegistrationResultPageItems(Repository repository) {
        String StorageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String key = String.format("%s-%s-", StorageId, repositoryId);
        Map<String, Map<String, RegistrationResultPageItem>> result = new HashMap<>();
        for (Map.Entry<String, Map<String, RegistrationResultPageItem>> entry : mapV3.entrySet()) {
            String mapKey = entry.getKey();
            if (mapKey.startsWith(key)) {
                String lower_id = mapKey.substring(key.length());
                result.put(lower_id, entry.getValue());
            }
        }
        return result;
    }

    /**
     * 获取所有的Entry属性(V2)
     * @param repository
     * @return <packageId, <version, Entry>>
     */
    public Map<String, Map<String, Entry>> getAllFeedEntries(Repository repository) {
        String StorageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String key = String.format("%s-%s-", StorageId, repositoryId);
        Map<String, Map<String, Entry>> result = new HashMap<>();
        for (Map.Entry<String, Map<String, Entry>> mapEntry : mapV2.entrySet()) {
            String mapKey = mapEntry.getKey();
            if (mapKey.startsWith(key)) {
                String lower_id = mapKey.substring(key.length());
                result.put(lower_id, mapEntry.getValue());
            }
        }
        return result;
    }

    /**
     * 获取指定packageId的所有properties
     * @param repository
     * @param lower_id
     * @return
     */
    public List<Entry> getFeedEntries(Repository repository, String lower_id) {
        String key = getKey(repository, lower_id);
        Map<String, Entry> entryMap = mapV2.get(key);
        ArrayList<Entry> entryList = new ArrayList<>();
        if (entryMap == null) {
            return entryList;
        }
        for (Map.Entry<String, Entry> entry : entryMap.entrySet()) {
            Entry entryValue = entry.getValue();
            entryList.add(entryValue);
        }
        return entryList;
    }

    public boolean exists(Repository repository, String lower_id) {
        String key = getKey(repository,lower_id);
        return mapV3.containsKey(key);
    }

    public List<RegistrationResultPageItem> getRegistrationResultPageItem(Repository repository, String lower_id) {
        String key = getKey(repository,lower_id);
        Map<String, RegistrationResultPageItem> versionMap = mapV3.get(key);
        if (versionMap == null) {
            return null;
        }
        ArrayList<RegistrationResultPageItem> registrationResultPageItems = new ArrayList<>();
        for (Map.Entry<String, RegistrationResultPageItem> entry : versionMap.entrySet()) {
            RegistrationResultPageItem registrationResultPageItem = entry.getValue();
            registrationResultPageItems.add(registrationResultPageItem);
        }
        return registrationResultPageItems;
    }

    public void remove(Repository repository, String lower_id, String version) {
        String key = getKey(repository,lower_id);
        if (mapV3.containsKey(key)) {
            Map<String, RegistrationResultPageItem> versionMap = mapV3.get(key);
            if (versionMap == null) {
                return;
            }
            versionMap.remove(version);
            if (versionMap.isEmpty()) {
                mapV3.remove(key);
            } else {
                mapV3.put(key, versionMap);
            }
        }
        if (mapV2.containsKey(key)) {
            Map<String, Entry> entryMap = mapV2.get(key);
            if (entryMap == null) {
                return;
            }
            entryMap.remove(version);
            if (entryMap.isEmpty()) {
                mapV2.remove(key);
            } else {
                mapV2.put(key, entryMap);
            }
        }
    }

    public Entry getFeedEntry(Repository repository, String lower_id, String version) {
        String key = getKey(repository,lower_id);
        Map<String, Entry> entryMap = mapV2.get(key);
        if (entryMap == null) {
            return null;
        }
        return entryMap.get(version);
    }

    private String getKey(Repository repository, String lower_id) {
        lower_id = lower_id.toLowerCase();
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        return String.format("%s-%s-%s", storageId, repositoryId, lower_id);
    }
}
