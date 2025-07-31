package com.folib.service;

import com.folib.cache.NugetPackageCache;
import com.folib.nuget.indexer.model.NugetMetadata;
import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.odata.feed.Properties;
import com.folib.nuget.odata.utils.NugetPropertiesBuilder;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nuget.utils.NugetSemVerUtil;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.registration.NugetV3RegistrationUtil;
import com.folib.storage.repository.Repository;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
@Slf4j
public class NugetCacheService {
    private final NugetPackageCache nugetPackageCache;

    @Autowired
    public NugetCacheService(NugetPackageCache registrationResultPageItemCache) {
        this.nugetPackageCache = registrationResultPageItemCache;
    }

    public void cachePackage(Repository repository, @NonNull NugetMetadata nugetMetadata) {
        String packageId = nugetMetadata.getId();
        String lower_id = packageId.toLowerCase();
        String version = nugetMetadata.getVersion();
        String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
        boolean isSemVer2 = NugetSemVerUtil.isSemVerLevel2(version);
        String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetV3RegistrationBaseUrl(repository, isSemVer2);
        RegistrationResultPageItem registrationResultPageItem = NugetV3RegistrationUtil.metadataToRegistrationResultPageItem(nugetMetadata, v3RegistrationBaseUrl, v2BaseUrl);
        Properties properties = NugetPropertiesBuilder.buildProperties(nugetMetadata);
        Entry entry = new Entry(properties, v2BaseUrl);
        nugetPackageCache.put(repository, lower_id, version, registrationResultPageItem, entry);
    }

    public void deCachePackage(Repository repository, String packageId, String version) {
        String lower_id = packageId.toLowerCase();
        nugetPackageCache.remove(repository, lower_id, version);

    }
}
