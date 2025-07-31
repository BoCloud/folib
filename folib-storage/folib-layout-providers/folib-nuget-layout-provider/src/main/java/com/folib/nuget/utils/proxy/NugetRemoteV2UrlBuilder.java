package com.folib.nuget.utils.proxy;

import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.storage.repository.Repository;
import com.folib.utils.PathUtils;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;


@Slf4j
public class NugetRemoteV2UrlBuilder {

    public static Exception getRemoteRepositoryUrlException = new Exception("Failed to get remote repository URL. Ensure the repository is a proxy repository.");

    @NonNull
    public static String getRemoteRepositoryUrl(@NonNull Repository repository) throws Exception {
        if (!repository.isProxyRepository()) {
            log.error("Repository {} is not a proxy repository.", repository.getId());
            throw getRemoteRepositoryUrlException;
        }
        String url = repository.getRemoteRepository().getUrl();
        url = PathUtils.trimTrailingSlashes(url);
        return url;
    }

    @NonNull
    public static String nugetFindPackageByIdUrl(@NonNull Repository repository, @NonNull String packageId, @NonNull String semVerLevel) throws Exception {
        String baseUrl = getRemoteRepositoryUrl(repository);
        String findUrl = String.format("/FindPackagesById()?id='%s'&semVerLevel='%s'", packageId, semVerLevel);
        return baseUrl + findUrl;
    }

    @NonNull
    public static String nugetEntryUrl(@NonNull Repository repository, @NonNull String packageId, @NonNull String version) throws Exception {
        String baseUrl = getRemoteRepositoryUrl(repository);
        String entryUrl = String.format("Packages(Id='%s',Version='%s')", packageId, version);
        return baseUrl + entryUrl;
    }

    @NonNull
    public static String nugetSearchUrl(@NonNull Repository repository, @NonNull NugetSearchRequest searchRequest) throws Exception {
        String baseUrl = getRemoteRepositoryUrl(repository);
        String searchUrl = "/Search()";
        String urlParameters = searchRequest.toUrlParametersV2();
        return baseUrl + searchUrl + "?" + urlParameters;
    }

    @NonNull
    public static String nugetSearchCountUrl(@NonNull Repository repository, @NonNull NugetSearchRequest searchRequest) throws Exception {
        String baseUrl = getRemoteRepositoryUrl(repository);
        String searchCountUrl = "/SearchCount()";
        String urlParameters = searchRequest.toUrlParametersV2();
        return baseUrl + searchCountUrl + "?" + urlParameters;
    }

    @NonNull
    public static String nugetSymbolsUrl(@NonNull Repository repository, @NonNull String packageId, @NonNull String version) throws Exception {
        String baseUrl = getRemoteRepositoryUrl(repository);
        String symbolsUrl = String.format("/%s/%s", packageId, version);
        return baseUrl + symbolsUrl;
    }

    @NonNull
    public static String nugetSymbolPdbUrl(@NonNull Repository repository, @NonNull String packageId, @NonNull String guid) throws Exception {
        String baseUrl = getRemoteRepositoryUrl(repository);
        String symbolsUrl = String.format("/%s.pdb/%s/%s.pdb", packageId, guid, packageId);
        return baseUrl + symbolsUrl;
    }
}
