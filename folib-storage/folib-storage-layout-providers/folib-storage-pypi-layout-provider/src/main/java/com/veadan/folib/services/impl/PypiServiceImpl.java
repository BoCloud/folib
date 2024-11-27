package com.veadan.folib.services.impl;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.enums.PypiRepositoryTypeEnum;
import com.veadan.folib.services.PypiProvider;
import com.veadan.folib.services.PypiService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.inject.Inject;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class PypiServiceImpl implements PypiService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private PypiProviderRegistry pypiProviderRegistry;

    @Override
    public String packages(Repository repository, String packageName, String targetUrl) {
        PypiProvider pypiProvider = pypiProviderRegistry.getProvider(PypiRepositoryTypeEnum.resolveType(repository.getType()));
        return pypiProvider.packages(repository, packageName, targetUrl);
    }
}
