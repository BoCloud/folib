package com.folib.nugetv3.search;

import com.folib.cache.NugetPackageCache;
import com.folib.nuget.filter.NuGetSearchFilter;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;



@Component
public class NugetV3HostSearchUtil {
    private final NugetPackageCache nugetPackageCache;

    @Autowired
    public NugetV3HostSearchUtil(NugetPackageCache nugetPackageCache) {
        this.nugetPackageCache = nugetPackageCache;
    }

    /**
     * 直接从缓存中进行检索
     *
     * @param searchRequest
     * @return
     */
    public Map<String, Map<String, RegistrationResultPageItem>> searchHostItems(NugetSearchRequest searchRequest) {
        // 1. 从缓存中获取所有的注册页项
        // <id, <version, RegistrationResultPageItem>>
        Map<String, Map<String, RegistrationResultPageItem>> allPackages = nugetPackageCache.getAllRegistrationResultPageItems(searchRequest.getRepository());

        // 2. 过滤(使用NuGetSearchFilter.accept方法)
        NuGetSearchFilter nuGetSearchFilter = new NuGetSearchFilter(searchRequest);
        for (Map.Entry<String, Map<String, RegistrationResultPageItem>> entry : allPackages.entrySet()) {
            String packageId = entry.getKey();
            Map<String, RegistrationResultPageItem> versionMap = entry.getValue();

            // 过滤掉不符合条件的版本
            versionMap.entrySet().removeIf(versionEntry -> {
                String version = versionEntry.getKey();
                RegistrationResultPageItem item = versionEntry.getValue();
                return !nuGetSearchFilter.accepts(packageId, version, item.getCatalogEntry().getDescription());
            });
            // 如果版本映射为空，则删除该包
            if (versionMap.isEmpty()) {
                allPackages.remove(packageId);
            }
        }
        return allPackages;
    }
}
