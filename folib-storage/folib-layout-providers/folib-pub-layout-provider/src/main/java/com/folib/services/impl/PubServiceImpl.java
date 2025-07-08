package com.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.folib.configuration.ConfigurationManager;
import com.folib.domain.PubPackageVersionMetadata;
import com.folib.enums.PubRepositoryTypeEnum;
import com.folib.services.PubProvider;
import com.folib.services.PubService;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.inject.Inject;

/**
 * @author veadan
 **/
@Slf4j
@Service
public class PubServiceImpl implements PubService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private PubProviderRegistry pubProviderRegistry;

    @Override
    public PubPackageVersionMetadata inspectVersion(Repository repository, String packageName, String version, String targetUrl) {
        PubProvider pubProvider = pubProviderRegistry.getProvider(PubRepositoryTypeEnum.resolveType(repository.getType()));
        return pubProvider.inspectVersion(repository, packageName, version, targetUrl);
    }

    @Override
    public JSONObject packages(Repository repository, String packageName, String targetUrl) {
        PubProvider pubProvider = pubProviderRegistry.getProvider(PubRepositoryTypeEnum.resolveType(repository.getType()));
        return pubProvider.packages(repository, packageName, targetUrl);
    }
}
