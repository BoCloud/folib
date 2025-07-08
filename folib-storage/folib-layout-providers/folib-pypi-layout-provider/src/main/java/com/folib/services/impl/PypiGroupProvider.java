package com.folib.services.impl;

import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.enums.PypiRepositoryTypeEnum;
import com.folib.services.PypiProvider;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.List;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PypiGroupProvider implements PypiProvider {

    @Inject
    private PypiProviderRegistry pypiProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @PostConstruct
    @Override
    public void register() {
        pypiProviderRegistry.addProvider(PypiRepositoryTypeEnum.PYPI_GROUP.getType(), this);
        log.info("Registered pypi provider '[{}]' with alias '[{}]'.", getClass().getCanonicalName(), PypiRepositoryTypeEnum.PYPI_GROUP.getType());

    }

    @Override
    public String packages(Repository repository, String packageName, String targetUrl) {
        String htmlData = null, subHtmlData;
        htmlData = getLocalPackages(repository, packageName, targetUrl);
        if (StringUtils.isNotBlank(htmlData)) {
            return htmlData;
        }
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                PypiProvider pypiProvider = pypiProviderRegistry.getProvider(PypiRepositoryTypeEnum.resolveType(subRepository.getType()));
                subHtmlData = pypiProvider.packages(subRepository, packageName, targetUrl);
                if (StringUtils.isNotBlank(subHtmlData)) {
                    htmlData = subHtmlData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return htmlData;
    }

    @Override
    public String getLocalPackages(Repository repository, String packageName, String targetUrl) {
        String htmlData = null, subHtmlData;
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                PypiProvider pypiProvider = pypiProviderRegistry.getProvider(PypiRepositoryTypeEnum.resolveType(subRepository.getType()));
                subHtmlData = pypiProvider.getLocalPackages(subRepository, packageName, targetUrl);
                if (StringUtils.isNotBlank(subHtmlData)) {
                    htmlData = subHtmlData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return htmlData;
    }

    public boolean isRepositoryResolvable(Repository repository) {
        final boolean isInService = repository.isInService();
        if (!isInService) {
            log.info("- Repository [{}] is not in service, skipping...",
                    repository.getStorageIdAndRepositoryId());
            return false;
        }
        return true;
    }

}
