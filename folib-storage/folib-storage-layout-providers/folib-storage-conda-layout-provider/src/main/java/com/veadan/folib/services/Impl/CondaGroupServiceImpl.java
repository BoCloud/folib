package com.veadan.folib.services.Impl;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.event.CondaRepodataEvent;
import com.veadan.folib.index.model.RepoData;
import com.veadan.folib.index.model.RepoDataEventKind;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.CondaGroupService;
import com.veadan.folib.services.CondaRepoDataService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.lang.reflect.ParameterizedType;
import java.nio.file.Files;
import java.util.*;

/**
 * @author LingengMa
 * @date 2025/04/22 14:17
 * @Description:
 */

@Slf4j
@Service
public class CondaGroupServiceImpl implements CondaGroupService {

    private final RepositoryPathResolver repositoryPathResolver;

    private final CondaRepoDataService condaRepoDataService;

    private final ConfigurationManager configurationManager;

    @Autowired
    public CondaGroupServiceImpl(RepositoryPathResolver repositoryPathResolver, CondaRepoDataServiceImpl condaRepoDataServiceImpl, CondaRepoDataService condaRepoDataService, ConfigurationManager configurationManager) {
        this.repositoryPathResolver = repositoryPathResolver;
        this.condaRepoDataService = condaRepoDataService;
        this.configurationManager = configurationManager;
    }

    @Override
    public void aggregateCondaGroupRepoData(Repository groupRepository, Repository sonRepository) {
        RepositoryPath groupPath = repositoryPathResolver.resolve(groupRepository);
        RepositoryPath sonPath = repositoryPathResolver.resolve(sonRepository);
        File groupDir = new File(groupPath.toString());
        File sonDir = new File(sonPath.toString());

        // 1. 遍历两个path下的所有子目录 -> List<String> platformList
        Set<String> platformList = new HashSet<>();
        for (File file : Objects.requireNonNull(groupDir.listFiles())) {
            if (file.isDirectory()) {
                platformList.add(file.getName());
            }
        }
        for (File file : Objects.requireNonNull(sonDir.listFiles())) {
            if (file.isDirectory()) {
                platformList.add(file.getName());
            }
        }

        // 2. 遍历platformList, 对每个平台进行处理
        for (String platform : platformList) {
            // 获取两个仓库的平台索引
            RepoData sonRepoData = condaRepoDataService.getRepoData(sonRepository, platform);
            // 合并索引
            CondaRepodataEvent event = new CondaRepodataEvent(RepoDataEventKind.AGGREGATE, groupRepository, platform,
                    sonRepoData);
            condaRepoDataService.sendRepoDataEvent(event);
        }
    }


    @Override
    public void aggregateCondaGroupRepoData(Repository groupRepository) {
        if (!groupRepository.isGroupRepository()) {
            throw new IllegalArgumentException("The repository is not a group repository");
        }
        for (String id: groupRepository.getGroupRepositories()) {
            Repository subRepository = configurationManager.getRepository(id);
            if (subRepository == null) {
                continue;
            }
            aggregateCondaGroupRepoData(subRepository, groupRepository);
        }
    }
}
