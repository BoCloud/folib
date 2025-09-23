package com.folib.nuget.utils.proxy;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.cache.NugetRemoteUrlCache;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nugetv3.model.index.NugetServiceIndex;
import com.folib.nugetv3.model.index.Resource;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.repository.Repository;
import com.folib.utils.PathUtils;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;



@Slf4j
public class NugetRemoteV3UrlBuilder {
    public static String[] REGISTRATION_RESOURCE_TYPES = {
            "RegistrationBaseUrl",
            "RegistrationsBaseUrl/3.0.0-beta",
            "RegistrationsBaseUrl/3.0.0-rc",
    };

    public static String REGISTRATION_SEMVER2_RESOURCE_TYPE = "RegistrationsBaseUrl/3.6.0";

    public static String PACKAGE_BASE_ADDRESS_RESOURCE_TYPE = "PackageBaseAddress/3.0.0";

    public static String SEARCH_QUERY_SERVICE_RESOURCE_TYPE = "SearchQueryService";


    @NonNull
    public static String getRemoteServiceIndexUrl(@NonNull Repository repository) {
        if (!repository.isProxyRepository()) {
            log.error("Repository {} is not a proxy repository.", repository.getId());
            return null;
        }
        String url = repository.getRemoteRepository().getUrl();
        url = PathUtils.trimTrailingSlashes(url);
        return url;
    }

    @NonNull
    public static NugetServiceIndex getNugetProxyServiceIndex(Repository repository) throws Exception {
        RepositoryPathResolver repositoryPathResolver = SpringUtil.getBean(RepositoryPathResolver.class);
        RepositoryPath indexPath = repositoryPathResolver.resolve(repository, "index.json");
        String remoteUrl = NugetRemoteV3UrlBuilder.getRemoteServiceIndexUrl(repository);
        return NugetRemoteUtil.fetchAndCache(remoteUrl, NugetServiceIndex.class, indexPath, FetchType.JSON);
    }

    @NonNull
    public static String getRemoteRegistrationUrl(@NonNull Repository repository, @NonNull String packageId, boolean isSemVer2Endpoint) throws Exception {
        NugetRemoteUrlCache nugetRemoteUrlCache = SpringUtil.getBean(NugetRemoteUrlCache.class);
        String result = nugetRemoteUrlCache.getRegistration(repository, packageId);
        if (result != null) {
            return result;
        }
        String baseUrl = getRegistrationId(repository, isSemVer2Endpoint);
        baseUrl = PathUtils.trimTrailingSlashes(baseUrl);
        return String.format("%s/%s/index.json", baseUrl, packageId);
    }

    @NonNull
    public static String getRemoteRegistrationPageUrl(@NonNull Repository repository, @NonNull String packageId, @NonNull String lower, @NonNull String upper, boolean isSemVer2Endpoint) throws Exception {
        NugetRemoteUrlCache nugetRemoteUrlCache = SpringUtil.getBean(NugetRemoteUrlCache.class);
        String url = nugetRemoteUrlCache.getPage(repository, packageId, lower, upper, isSemVer2Endpoint);
        if (url != null) {
            return url;
        }
        url  = nugetRemoteUrlCache.getPage(repository, packageId, "@@", "##", isSemVer2Endpoint);
        url = url.replace("@@", lower)
                 .replace("##", upper);
        return url;
    }


    @NonNull
    public static String getRemoteSearchUrl(@NonNull Repository repository, @NonNull NugetSearchRequest searchRequest) throws Exception{
        String id = getSearchQueryServiceId(repository);
        id = PathUtils.trimTrailingSlashes(id);
        String urlParameters = searchRequest.toUrlParametersV3();
        return id + '?' + urlParameters;
    }

    @NonNull
    public static String getRemotePackageDownloadUrl(@NonNull Repository repository, @NonNull String packageId, @NonNull String version) throws Exception {
        String id = getPackageBaseAddressId(repository);
        id = PathUtils.trimTrailingSlashes(id);
        packageId = packageId.toLowerCase();
        return String.format("%s/%s/%s/%s.%s.nupkg", id, packageId, version, packageId, version);
    }


    private static Resource getResource(Repository repository, String resourceType) throws Exception {
        NugetServiceIndex nugetServiceIndex = getNugetProxyServiceIndex(repository);
        return nugetServiceIndex.getResources().stream()
                .filter(resource -> resource.getType().equals(resourceType))
                .findFirst()
                .orElseThrow(() -> new Exception("Resource type " + resourceType + " not found in NuGet service index."));
    }

    private static String getRegistrationId(Repository repository, boolean isSemVer2Endpoint) throws Exception {
        if (isSemVer2Endpoint) {
            return getResource(repository, REGISTRATION_SEMVER2_RESOURCE_TYPE).getId();
        }
        for (String resourceType : REGISTRATION_RESOURCE_TYPES) {
            try {
                return getResource(repository, resourceType).getId();
            } catch (Exception e) {
                log.warn("Resource type {} not found in NuGet service index. Trying next.", resourceType);
            }
        }
        throw new Exception("No suitable registration resource found in NuGet service index for repository " + repository.getId());
    }

    private static String getPackageBaseAddressId(Repository repository) throws Exception {
        return getResource(repository, PACKAGE_BASE_ADDRESS_RESOURCE_TYPE).getId();
    }

    private static String getSearchQueryServiceId(Repository repository) throws Exception {
        return getResource(repository, SEARCH_QUERY_SERVICE_RESOURCE_TYPE).getId();
    }

}
