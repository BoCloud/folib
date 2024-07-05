package com.veadan.folib.services.impl;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.enums.PypiRepositoryTypeEnum;
import com.veadan.folib.services.PypiProvider;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author leipenghui
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
        for (String storageAndRepositoryId : repository.getGroupRepositories()) {
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
