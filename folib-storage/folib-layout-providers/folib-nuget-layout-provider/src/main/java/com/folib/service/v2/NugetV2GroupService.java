package com.folib.service.v2;

import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.odata.feed.Feed;
import com.folib.nuget.search.NugetFeedUtil;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nuget.utils.NugetRepositoryUtil;
import com.folib.nuget.utils.NugetSearchRequestUtil;
import com.folib.service.NugetV2Service;
import com.folib.storage.repository.Repository;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;



@Slf4j
@Component
public class NugetV2GroupService implements NugetV2Service {
    private final NugetV2HostService hostService;
    private final NugetV2RemoteService remoteService;

    public NugetV2GroupService(NugetV2HostService hostService, NugetV2RemoteService remoteService) {
        this.hostService = hostService;
        this.remoteService = remoteService;
    }


    @Override
    public String getType() {
        return "GROUP";
    }

    public Feed searchWithoutPage(NugetSearchRequest searchRequest) throws Exception {
        Repository repository = searchRequest.getRepository();
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        List<Feed> feeds = new ArrayList<>();
        // 1. 获取子仓库所有的搜索结果
        for (Repository subRepo : subRepos) {
            try {
                Feed feed = null;
                NugetSearchRequest subRequest = NugetSearchRequestUtil.subSearchRequestWithoutPage(searchRequest, subRepo);
                if (subRepo.isHostedRepository()) {
                    feed = hostService.search(subRequest);
                } else if (subRepo.isProxyRepository()) {
                    feed = remoteService.search(subRequest);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (feed != null) {
                    feeds.add(feed);
                }
            } catch (Exception e) {
                // 记录错误日志
                e.printStackTrace();
            }
        }
        // 2. 合并所有的搜索结果
        String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
        Feed resultFeed = NugetFeedUtil.mergeFeeds(feeds, v2BaseUrl, "title");
        return resultFeed;
    }

    @Override
    public Feed search(NugetSearchRequest searchRequest) throws Exception {
        if (searchRequest.getTake() == null) {
            return searchWithoutPage(searchRequest);
        }
        if (searchRequest.getSkip() == null) {
            searchRequest.setSkip(0);
        }
        Repository repository = searchRequest.getRepository();
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        List<Feed> feeds = new ArrayList<>();
        int skip = searchRequest.getSkip();
        int take = searchRequest.getTake();
        for (Repository subRepo : subRepos) {
            if (take <= 0) {
                break; // 如果已经获取到足够的结果，直接跳出循环
            }
            try {
                // 1. 获取子仓库的搜索结果数量(不分页)
                NugetSearchRequest subRequest = NugetSearchRequestUtil.subSearchRequestWithoutPage(searchRequest, subRepo);
                int count = 0;
                if (subRepo.isHostedRepository()) {
                    count = hostService.searchCount(subRequest);
                } else if (subRepo.isProxyRepository()) {
                    count = remoteService.searchCount(subRequest);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (count == 0) {
                    continue; // 如果子仓库没有搜索结果，跳过
                }
                if (skip >= count) {
                    skip -= count; // 如果跳过的数量大于等于子仓库的数量，继续跳过
                    continue;
                }
                int subTake = Math.min(take, count - skip);
                subRequest = NugetSearchRequestUtil.subSearchRequest(searchRequest, subRepo, skip, subTake);
                // 2. 分页获取子仓库的搜索结果
                Feed feed = null;
                if (subRepo.isHostedRepository()) {
                    feed = hostService.search(subRequest);
                } else if (subRepo.isProxyRepository()) {
                    feed = remoteService.search(subRequest);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (feed != null) {
                    feeds.add(feed);
                    take -= subTake; // 减去已获取的数量
                    if (skip > 0) {
                        skip = 0;
                    }
                }
            } catch (Exception e) {
                // 记录错误日志
                e.printStackTrace();
            }
        }
        // 2. 合并所有的搜索结果
        String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
        Feed resultFeed = NugetFeedUtil.mergeFeeds(feeds, v2BaseUrl, "title");
        return resultFeed;
    }

    @Override
    public Feed findPackageById(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        List<Feed> feeds = new ArrayList<>();
        // 1. 获取子仓库所有的包信息
        for (Repository subRepo : subRepos) {
            try {
                Feed feed = null;
                if (subRepo.isHostedRepository()) {
                    feed = hostService.findPackageById(subRepo, packageId, isSemVer2Endpoint);
                } else if (subRepo.isProxyRepository()) {
                    feed = remoteService.findPackageById(subRepo, packageId, isSemVer2Endpoint);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (feed != null) {
                    feeds.add(feed);
                }
            } catch (Exception e) {
                // 记录错误日志
                e.printStackTrace();
            }
        }
        // 2. 合并所有的包信息
        String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
        Feed resultFeed = NugetFeedUtil.mergeFeeds(feeds, v2BaseUrl, "title");
        return resultFeed;
    }

    @Override
    public Entry packageEntry(Repository repository, String packageId, String version) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        for (Repository subRepo : subRepos) {
            try {
                Entry entry = null;
                if (subRepo.isHostedRepository()) {
                    entry = hostService.packageEntry(subRepo, packageId, version);
                } else if (subRepo.isProxyRepository()) {
                    entry = remoteService.packageEntry(subRepo, packageId, version);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (entry != null) {
                    // 重写URL
                    String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
                    return entry;
                }
            } catch (Exception e) {
                // 记录错误日志
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public void provideDownloadNupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        for (Repository subRepo : subRepos) {
            try {
                if (subRepo.isHostedRepository()) {
                    hostService.provideDownloadNupkg(subRepo, packageId, version, response);
                } else if (subRepo.isProxyRepository()) {
                    remoteService.provideDownloadNupkg(subRepo, packageId, version, response);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                return; // 成功下载后直接返回
            } catch (Exception e) {
                // 记录错误日志
                e.printStackTrace();
            }
        }
    }

    @Override
    public void provideDownloadSymbolPdb(Repository repository, String packageId, String guid, HttpServletResponse response, HttpHeaders httpHeaders) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        for (Repository subRepo : subRepos) {
            try {
                if (subRepo.isHostedRepository()) {
                    hostService.provideDownloadSymbolPdb(subRepo, packageId, guid, response, httpHeaders);
                } else if (subRepo.isProxyRepository()) {
                    remoteService.provideDownloadSymbolPdb(subRepo, packageId, guid, response, httpHeaders);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                return; // 成功下载后直接返回
            } catch (Exception e) {
                // 记录错误日志
                e.printStackTrace();
            }
        }

    }

    @Override
    public void provideDownloadSnupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        for (Repository subRepo : subRepos) {
            try {
                if (subRepo.isHostedRepository()) {
                    hostService.provideDownloadSnupkg(subRepo, packageId, version, response);
                } else if (subRepo.isProxyRepository()) {
                    remoteService.provideDownloadSnupkg(subRepo, packageId, version, response);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                return; // 成功下载后直接返回
            } catch (Exception e) {
                // 记录错误日志
                e.printStackTrace();
            }
        }
    }


    // 可以优化
    @Override
    public int searchCount(NugetSearchRequest searchRequest) {
        try {
            Feed feed = this.search(searchRequest);
            return feed.getEntries().size();
        } catch (Exception e) {
            return 0;
        }
    }
}
