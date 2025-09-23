package com.folib.nuget.search;

import com.folib.cache.NugetPackageCache;
import com.folib.nuget.filter.NuGetSearchFilter;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.odata.feed.Entry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;



@Component
public class NugetV2HostSearchUtil {
    private final NugetPackageCache registrationResultPageItemCache;

    @Autowired
    public NugetV2HostSearchUtil(NugetPackageCache registrationResultPageItemCache) {
        this.registrationResultPageItemCache = registrationResultPageItemCache;
    }

    /**
     * 直接从缓存中进行检索
     *
     * @param searchRequest
     * @return
     */
    public Map<String, Map<String, Entry>> searchHostItems(NugetSearchRequest searchRequest) {
        // 1. 从缓存中获取所有的注册页项
        // <id, <version, Properties>>
        Map<String, Map<String, Entry>> allPackages = registrationResultPageItemCache.getAllFeedEntries(searchRequest.getRepository());

        // 2. 过滤(使用NuGetSearchFilter.accept方法)
        NuGetSearchFilter nuGetSearchFilter = new NuGetSearchFilter(searchRequest);
        for (Map.Entry<String, Map<String, Entry>> mapEntry : allPackages.entrySet()) {
            String packageId = mapEntry.getKey();
            Map<String, Entry> versionMap = mapEntry.getValue();

            // 过滤掉不符合条件的版本
            versionMap.entrySet().removeIf(versionEntry -> {
                String version = versionEntry.getKey();
                Entry entry = versionEntry.getValue();
                boolean res = !nuGetSearchFilter.accepts(packageId, version, entry.getProperties().getDescription());
                return res;
            });
            // 如果版本映射为空，则删除该包
            if (versionMap.isEmpty()) {
                allPackages.remove(packageId);
            }
        }
        return allPackages;
    }
}
