package com.folib.nuget.utils;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.configuration.ConfigurationUtils;
import com.folib.services.RepositoryManagementService;
import com.folib.storage.repository.Repository;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;




public class NugetRepositoryUtil {
    public static List<Repository> getSubRepositoriesHostAndProxy(Repository repository) {
        String groupStorageId = repository.getStorage().getId();
        Set<Repository> subRepositories = new HashSet<>();
        for (String storageAndRepo : repository.getGroupRepositories()) {
            String storageId = ConfigurationUtils.getStorageId(groupStorageId, storageAndRepo);
            String repositoryId = ConfigurationUtils.getRepositoryId(storageAndRepo);
            RepositoryManagementService repositoryManagementService = SpringUtil.getBean(RepositoryManagementService.class);
            Repository subRepository = repositoryManagementService.getStorage(storageId).getRepository(repositoryId);
            if (subRepository != null) {
                if (subRepository.isHostedRepository() || subRepository.isProxyRepository()) {
                    subRepositories.add(subRepository);
                } else {
                    List<Repository> subSubRepositories = getSubRepositoriesHostAndProxy(subRepository);
                    for (Repository subSubRepo : subSubRepositories) {
                        if (subSubRepo.isHostedRepository() || subSubRepo.isProxyRepository()) {
                            subRepositories.add(subSubRepo);
                        }
                    }
                }
            }
        }
        return new ArrayList<>(subRepositories);
    }
}
