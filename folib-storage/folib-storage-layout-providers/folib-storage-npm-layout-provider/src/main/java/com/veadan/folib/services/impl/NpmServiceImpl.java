package com.veadan.folib.services.impl;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.enums.NpmRepositoryTypeEnum;
import com.veadan.folib.npm.metadata.PackageFeed;
import com.veadan.folib.npm.metadata.PackageVersion;
import com.veadan.folib.services.NpmProvider;
import com.veadan.folib.services.NpmService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.inject.Inject;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class NpmServiceImpl implements NpmService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private NpmProviderRegistry npmProviderRegistry;

    @Override
    public PackageVersion packageVersion(Repository repository, String packageName, String version, String targetUrl) {
        NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(repository.getType()));
        return npmProvider.packageVersion(repository, packageName, version, targetUrl);
    }

    @Override
    public PackageFeed packageFeed(Repository repository, String packageName, String targetUrl) {
        NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(repository.getType()));
        return npmProvider.packageFeed(repository, packageName, targetUrl);
    }

    @Override
    public String binary(Repository repository, String packageName, String targetUrl) {
        NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(repository.getType()));
        return npmProvider.binary(repository, packageName, targetUrl);
    }
}
