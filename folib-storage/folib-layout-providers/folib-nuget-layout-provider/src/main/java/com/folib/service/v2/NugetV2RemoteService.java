package com.folib.service.v2;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.cache.NugetRemoteUrlCache;
import com.folib.cache.NugetSearchCountCache;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.indexer.symbols.NugetSymbolsIndexer;
import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.odata.feed.Feed;
import com.folib.nuget.utils.proxy.FetchType;
import com.folib.nuget.utils.proxy.NugetRemoteUtil;
import com.folib.nuget.utils.proxy.NugetRemoteV2UrlBuilder;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.NugetV2Service;
import com.folib.storage.repository.Repository;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;

import java.util.List;



@Slf4j
@Component
public class NugetV2RemoteService implements NugetV2Service {
    private final RepositoryPathResolver repositoryPathResolver;

    private final NugetRemoteUrlCache nugetRemoteUrlCache;

    private final NugetSearchCountCache nugetSearchCountCache;

    @Autowired
    public NugetV2RemoteService(RepositoryPathResolver repositoryPathResolver, NugetRemoteUrlCache nugetRemoteUrlCache, NugetSearchCountCache nugetSearchCountCache) {
        this.repositoryPathResolver = repositoryPathResolver;
        this.nugetRemoteUrlCache = nugetRemoteUrlCache;
        this.nugetSearchCountCache = nugetSearchCountCache;
    }

    @Override
    public String getType() {
        return "PROXY";
    }

    @Override
    public Feed search(NugetSearchRequest searchRequest) throws Exception {
        Repository repository = searchRequest.getRepository();
        // 1. 构造远端URL
        String remoteUrl = NugetRemoteV2UrlBuilder.nugetSearchUrl(repository, searchRequest);
        // 2. fetch
        Feed feed = NugetRemoteUtil.fetchFromRemote(remoteUrl, Feed.class, FetchType.XML);
        cacheFeed(repository, feed, null);
        return feed;
    }


    // 需缓存 packageFeed/{packageId}.xml and packageFeed-semver2/{packageId}.xml
    // 以及entry: {packageId}/{version}/entry.xml
    @Override
    public Feed findPackageById(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception {
        // 1. 构造本地缓存路径
        packageId = packageId.toLowerCase();
        String subPath = String.format("%s/%s.xml", isSemVer2Endpoint ? "packageFeed-semver2" : "packageFeed", packageId);
        RepositoryPath feedPath = repositoryPathResolver.resolve(repository, subPath);

        // 2. 构造远程URL
        String semVerLevel = isSemVer2Endpoint ? "2.0.0" : "1.0.0";
        String remoteUrl = NugetRemoteV2UrlBuilder.nugetFindPackageByIdUrl(repository, packageId, semVerLevel);
        Feed feed = NugetRemoteUtil.fetchAndCache(remoteUrl, Feed.class, feedPath, FetchType.XML);
        cacheFeed(repository, feed, packageId);
        return feed;
    }

    // 需缓存 {packageId}/{version}/entry.xml
    @Override
    public Entry packageEntry(Repository repository, String packageId, String version) throws Exception {
        // 1. 构造本地缓存路径
        String subPath = String.format("%s/%s/entry.xml", packageId, version);
        RepositoryPath entryPath = repositoryPathResolver.resolve(repository, subPath);
        // 2. 构造远程URL
        String remoteUrl = NugetRemoteV2UrlBuilder.nugetEntryUrl(repository, packageId, version);
        return NugetRemoteUtil.fetchAndCache(remoteUrl, Entry.class, entryPath, FetchType.XML);
    }

    @Override
    public void provideDownloadNupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception {
        // 1. 构造本地缓存路径
        NugetCoordinates coordinates = new NugetCoordinates(packageId, version);
        RepositoryPath nupkgPath = repositoryPathResolver.resolve(repository, coordinates);
        // 2. 构造远程URL
        String remoteUrl = nugetRemoteUrlCache.getContent(repository, packageId, version);
        // 3. 下载并缓存
        NugetRemoteUtil.fetchAndCachePackage(repository, remoteUrl, nupkgPath);
        // 4. 提供下载
        NugetV2HostService hostService = SpringUtil.getBean(NugetV2HostService.class);
        hostService.provideDownloadNupkg(repository, packageId, version, response);
    }

    @Override
    public void provideDownloadSymbolPdb(Repository repository, String packageId, String guid, HttpServletResponse response, HttpHeaders httpHeaders) throws Exception {
        // 1. 构造本地缓存路径
        String subPath = NugetSymbolsIndexer.getSymbolFilePathInCache(packageId, guid);
        RepositoryPath pdbPath = repositoryPathResolver.resolve(repository, subPath);
        // 2. 构造远程URL
        String remoteUrl = NugetRemoteV2UrlBuilder.nugetSymbolPdbUrl(repository, packageId, guid);
        // 3. 下载并缓存
        NugetRemoteUtil.fetchAndCachePdb(remoteUrl, pdbPath, httpHeaders);
        // 4. 提供下载
        NugetV2HostService hostService = SpringUtil.getBean(NugetV2HostService.class);
        hostService.provideDownloadSymbolPdb(repository, packageId, guid, response, httpHeaders);
    }

    @Override
    public void provideDownloadSnupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception {
        // 1. 构造本地缓存路径
        NugetCoordinates coordinates = new NugetCoordinates(packageId, version, NugetCoordinates.SYMBOL_EXTENSION);
        RepositoryPath snupkgPath = repositoryPathResolver.resolve(repository, coordinates);
        // 2. 构造远程URL
        String remoteUrl = NugetRemoteV2UrlBuilder.nugetSymbolsUrl(repository, packageId, version);
        // 3. 下载并缓存
        NugetRemoteUtil.fetchAndCachePackage(repository, remoteUrl, snupkgPath);
        // 4. 提供下载
        NugetV2HostService hostService = SpringUtil.getBean(NugetV2HostService.class);
        hostService.provideDownloadSnupkg(repository, packageId, version, response);
    }

    @Override
    public int searchCount(NugetSearchRequest searchRequest) {
        try {

            // 1. 检查缓存
            if (nugetSearchCountCache.containsV2(searchRequest)) {
                return nugetSearchCountCache.getV2(searchRequest);
            }
            // 2. 构造远程URL
            Feed feed = this.search(searchRequest);
            // 3. fetch
            int count = feed.getEntries().size();
            // 4. cache
            nugetSearchCountCache.putV2(searchRequest, count);
            return count;
        } catch (Exception e) {
            log.error("Error fetching search count for request: {}", searchRequest, e);
            return 0;
        }
    }


    private void cacheFeed(Repository repository, Feed feed, String packageId) throws Exception {
        List<Entry> entries = feed.getEntries();
        if (entries == null || entries.isEmpty()) {
            return;
        }

        for (Entry entry : entries) {
            if (packageId == null) {
                packageId = entry.getProperties().getId();
            }
            String version = entry.getProperties().getVersion();
            nugetRemoteUrlCache.putContent(repository, packageId, version, entry.getContent().getSrc());
        }
    }
}
