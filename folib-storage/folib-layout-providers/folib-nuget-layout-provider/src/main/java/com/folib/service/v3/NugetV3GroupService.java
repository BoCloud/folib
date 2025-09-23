package com.folib.service.v3;

import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nuget.utils.NugetRepositoryUtil;
import com.folib.nuget.utils.NugetSearchRequestUtil;
import com.folib.nugetv3.model.registration.RegistrationResult;
import com.folib.nugetv3.model.registration.RegistrationResultPage;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.model.search.SearchResult;
import com.folib.nugetv3.registration.NugetV3RegistrationUtil;
import com.folib.nugetv3.search.NugetSearchResultUtil;
import com.folib.service.NugetV3Service;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * @author LingengMa
 * @date 2025/06/05 13:58
 * @Description:
 */


@Component
@Slf4j
public class NugetV3GroupService implements NugetV3Service {
    private final NugetV3HostService hostService;
    private final NugetV3RemoteService remoteService;

    @Autowired
    public NugetV3GroupService(NugetV3HostService hostService, NugetV3RemoteService remoteService) {
        this.hostService = hostService;
        this.remoteService = remoteService;
    }

    @Override
    public String getType() {
        return "GROUP";
    }

    @Override
    public RegistrationResult getRegistration(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        List<RegistrationResultPage> pages = new ArrayList<>();
        // 1. 获取子仓库所有的注册信息
        for (Repository subRepo : subRepos) {
            try {
                RegistrationResult result = null;
                if (subRepo.isHostedRepository()) {
                    result = hostService.getRegistration(subRepo, packageId, isSemVer2Endpoint);
                } else if (subRepo.isProxyRepository()) {
                    result = remoteService.getRegistration(subRepo, packageId, isSemVer2Endpoint);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (result != null) {
                    pages.addAll(result.getItems());
                }
            } catch (Exception e) {
                log.error("Error fetching registration for package {} from repository {}", packageId, subRepo.getId(), e);
            }
        }
        // 2. 合并所有的注册信息
        String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetV3RegistrationBaseUrl(repository, isSemVer2Endpoint);
        return NugetV3RegistrationUtil.registrationResultPagesToRegistrationResult(pages, v3RegistrationBaseUrl);
    }

    @Override
    public RegistrationResultPage getRegistrationPage(Repository repository, String packageId, String lower, String upper, boolean isSemVer2Endpoint) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        List<RegistrationResultPageItem> items = new ArrayList<>();
        // 1. 获取子仓库所有的注册页
        for (Repository subRepo : subRepos) {
            try {
                RegistrationResultPage page = null;
                if (subRepo.isHostedRepository()) {
                    page = hostService.getRegistrationPage(subRepo, packageId, lower, upper, isSemVer2Endpoint);
                } else if (subRepo.isProxyRepository()) {
                    page = remoteService.getRegistrationPage(subRepo, packageId, lower, upper, isSemVer2Endpoint);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (page != null) {
                    items.addAll(page.getItems());
                }
            } catch (Exception e) {
                log.error("Error fetching registration for package {} from repository {}", packageId, subRepo.getId(), e);
            }
        }

        // 2. 合并所有的注册页
        String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetV3RegistrationBaseUrl(repository, isSemVer2Endpoint);
        return NugetV3RegistrationUtil.registrationResultPageItemsToRegistrationResultPage(items, packageId, lower, upper, v3RegistrationBaseUrl);
    }

    @Override
    public RegistrationResultPageItem getRegistrationPageItem(Repository repository, String packageId, String version) throws Exception {
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        for (Repository subRepo : subRepos) {
            try {
                RegistrationResultPageItem item = null;
                if (subRepo.isHostedRepository()) {
                    item = hostService.getRegistrationPageItem(subRepo, packageId, version);
                } else if (subRepo.isProxyRepository()) {
                    item = remoteService.getRegistrationPageItem(subRepo, packageId, version);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (item != null) {
                    return item;
                }
            } catch (Exception e) {
                log.error("Error fetching registration page item for package {} version {} from repository {}", packageId, version, subRepo.getId(), e);
            }
        }
        log.error("No registration page item found for package {} version {} in any sub-repository of {}", packageId, version, repository.getId());
        return null;
    }

    @Override
    public int searchCount(NugetSearchRequest searchRequest) {
        try {
            SearchResult result = this.search(searchRequest);
            return result.getTotalHits();
        } catch (Exception e) {
            log.error("Error fetching search count for request: {}", searchRequest, e);
            return 0;
        }
    }

    public SearchResult searchWithoutPage(NugetSearchRequest searchRequest) throws Exception {
        Repository repository = searchRequest.getRepository();
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        List<SearchResult> results = new ArrayList<>();
        // 1. 获取子仓库所有的搜索结果
        for (Repository subRepo : subRepos) {
            try {
                NugetSearchRequest subSearchRequest = NugetSearchRequestUtil.subSearchRequestWithoutPage(searchRequest, subRepo);
                SearchResult result = null;
                if (subRepo.isHostedRepository()) {
                    result = hostService.search(subSearchRequest);
                } else if (subRepo.isProxyRepository()) {
                    result = remoteService.search(subSearchRequest);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (result != null) {
                    results.add(result);
                }
            } catch (Exception e) {
                log.error("Error fetching search results from repository {}", subRepo.getId(), e);
            }
        }
        // 2. 合并所有的搜索结果
        return NugetSearchResultUtil.mergeSearchResults(results);
    }

    @Override
    public SearchResult search(NugetSearchRequest searchRequest) throws Exception {
        if (searchRequest.getTake() == null) {
            return searchWithoutPage(searchRequest);
        }
        if (searchRequest.getSkip() == null) {
            searchRequest.setSkip(0);
        }
        Repository repository = searchRequest.getRepository();
        List<Repository> subRepos = NugetRepositoryUtil.getSubRepositoriesHostAndProxy(repository);
        List<SearchResult> results = new ArrayList<>();
        int skip = searchRequest.getSkip();
        int take = searchRequest.getTake();
        // 1. 获取子仓库所有的搜索结果
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
                // 2. 构造搜索参数
                int subTake = Math.min(take, count - skip);
                subRequest = NugetSearchRequestUtil.subSearchRequest(searchRequest, subRepo, skip, subTake);
                NugetSearchRequest subSearchRequest = NugetSearchRequestUtil.subSearchRequestWithoutPage(searchRequest, subRepo);
                SearchResult result = null;
                if (subRepo.isHostedRepository()) {
                    result = hostService.search(subSearchRequest);
                } else if (subRepo.isProxyRepository()) {
                    result = remoteService.search(subSearchRequest);
                } else {
                    throw new Exception("Unsupported repository type: " + subRepo.getType());
                }
                if (result != null) {
                    results.add(result);
                    take -= subTake; // 减去已获取的数量
                    if (skip > 0) {
                        skip = 0; // 如果跳过的数量大于0，重置为0
                    }
                }
            } catch (Exception e) {
                log.error("Error fetching search results from repository {}", subRepo.getId(), e);
            }
        }
        // 2. 合并所有的搜索结果
        SearchResult searchResult = NugetSearchResultUtil.mergeSearchResults(results);
        // 3. 进行分页处理
        searchResult = NugetSearchResultUtil.page(searchResult, searchRequest.getSkip(), searchRequest.getTake());
        return searchResult;
    }
}