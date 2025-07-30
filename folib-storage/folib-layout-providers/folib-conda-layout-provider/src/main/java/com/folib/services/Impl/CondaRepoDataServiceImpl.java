package com.folib.services.Impl;

import cn.hutool.extra.spring.SpringUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.components.DistributedLockComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.event.CondaRepodataEvent;
import com.folib.event.index.IndexEventListenerRegistry;
import com.folib.event.index.IndexTypeEnum;
import com.folib.index.cache.CondaIndexCache;
import com.folib.index.indexer.CondaMetadataIndexer;
import com.folib.index.model.RepoData;
import com.folib.index.model.RepoDataEventKind;
import com.folib.index.model.RepoDataPackage;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.CondaRepoDataService;
import com.folib.storage.repository.Repository;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

@Service
public class CondaRepoDataServiceImpl implements CondaRepoDataService {
    private final CondaMetadataIndexer condaMetadataIndexer;

    private final DistributedLockComponent distributedLockComponent;

    private final ArtifactResolutionService artifactResolutionService;

    private final RepositoryPathResolver repositoryPathResolver;

    private final ConfigurationManager configurationManager;

    private final CondaIndexCache condaIndexCache;

    private final String REPODATA = "repodata.json";
    private final String CURRENT_REPODATA = "current_repodata.json";

    @Autowired
    public CondaRepoDataServiceImpl(CondaMetadataIndexer condaMetadataIndexer, DistributedLockComponent distributedLockComponent, ArtifactResolutionService artifactResolutionService, RepositoryPathResolver repositoryPathResolver, ConfigurationManager configurationManager, CondaIndexCache condaIndexCache) {
        this.condaMetadataIndexer = condaMetadataIndexer;
        this.distributedLockComponent = distributedLockComponent;
        this.artifactResolutionService = artifactResolutionService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.configurationManager = configurationManager;
        this.condaIndexCache = condaIndexCache;
    }


    @Override
    public RepoData getRepoData(Repository repository, String platformId) {
        return readRepoData(repository, platformId, REPODATA);
    }

    @Override
    public RepoData getCurrentRepoData(Repository repository, String platformId) {
        return condaMetadataIndexer.createNewRepoData(platformId);
    }

    @Override
    public void sendRepoDataEvent(CondaRepodataEvent event) {
        // 获取锁
        Repository repository = event.getRepository();
        String repoKey = String.format("%s/%s/%s", repository.getStorage().getId(), repository.getId(), event.getPlatformId());
        String key = String.format("CondaRepoData_%s", repoKey);
        if (distributedLockComponent.lock(key, GlobalConstants.WAIT_LOCK_TIME * GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                handleRepoDataEvent(repository, event);
            } finally {
                distributedLockComponent.unLock(key);
            }
        }
    }

    private void handleRepoDataEvent(Repository repository, CondaRepodataEvent event) {
        RepoDataEventKind kind = event.getType();
        String platformId = event.getPlatformId();
        String artifactName = event.getArtifactName();

        RepositoryPath repoPath = repositoryPathResolver.resolve(repository, platformId + "/" + artifactName);
        if (repository.isGroupRepository() && !Files.exists(repoPath)) {
            // 组仓库，且不存在文件，直接返回(后续获取时会自动更新)
            return;
        }

        RepoData repoData = getRepoData(repository, platformId);
        RepoData currentRepoData = getCurrentRepoData(repository, platformId);

        if (kind == RepoDataEventKind.ADD) {
            addPackage(repoData, currentRepoData, artifactName, event.getRepoDataPackage());
        } else if (kind == RepoDataEventKind.REMOVE) {
            removePackage(repoData, currentRepoData, platformId, artifactName);
        }else if (kind == RepoDataEventKind.AGGREGATE) {
            aggregatePackage(repoData, event.getRepoData());
        } else {
            throw new RuntimeException("Unsupported RepoDataEventKind: " + kind);
        }
        saveRepoData(repoData, repository, platformId, REPODATA);
        saveRepoData(currentRepoData, repository, platformId, CURRENT_REPODATA);
        // 组仓库更新
        if (kind == RepoDataEventKind.ADD || kind == RepoDataEventKind.AGGREGATE) {
            String storageId = repository.getStorage().getId();
            String repositoryId = repository.getId();
            //发送索引更新事件
            IndexEventListenerRegistry registry = SpringUtil.getBean(IndexEventListenerRegistry.class);
            registry.dispatchUpdateIndexEvent(storageId, repositoryId + ":" + platformId, IndexTypeEnum.CONDA);
        }
    }

    @Override
    public boolean checkPackageExistsInRepoData(RepoData repoData,
                                                String artifactName) {
        return condaMetadataIndexer.checkPackageExistsInRepoData(repoData, artifactName);
    }

    private void addPackage(RepoData repoData, RepoData currentRepoData, String artifactName,
                            RepoDataPackage repoDataPackage) {
        if (repoDataPackage == null) {
            throw new RuntimeException("Failed to get RepoDataPackage for artifact: " + artifactName);
        }
        condaMetadataIndexer.addPackageToRepodata(repoData, repoDataPackage, artifactName);
        condaMetadataIndexer.addPackageToCurrentRepoData(currentRepoData, repoData, repoDataPackage, artifactName);
    }

    /**
     * Remove package from repodata
     *
     * @param repoData
     * @param currentRepoData
     * @param artifactName
     * @return
     */
    private void removePackage(RepoData repoData, RepoData currentRepoData, String platformId, String artifactName) {
        if (!this.checkPackageExistsInRepoData(repoData, artifactName)) {
            return;
        }
        condaMetadataIndexer.removePackageFromRepodata(repoData, artifactName);
        RepoData newCurrentRepoData = condaMetadataIndexer.reindexCurrentRepoData(repoData, platformId);
        if (newCurrentRepoData == null) {
            throw new RuntimeException("Failed to reindex current repo data for artifact: " + artifactName);
        }
        currentRepoData.update(newCurrentRepoData);
    }

    private void reindexPackage(Repository repository, String platformId) {
        if (repository.isProxyRepository()) {
            // 代理库不需要重构索引
            return;
        } else if (repository.isGroupRepository()) {
            // 重构组合库
            RepoData newRepoData = aggregateCondaGroupPlatformRepoData(repository, platformId);
            saveRepoData(newRepoData, repository, platformId, REPODATA);

        } else if (repository.isHostedRepository()){
            // 重构本地库
            RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository, platformId);
            String repoKey = repoDataPath.toString();
            RepoData newRepoData = condaMetadataIndexer.reindexRepoData(repoKey, platformId);
            if (newRepoData == null) {
                throw new RuntimeException("Failed to reindex repo data for repoKey.");
            }
            saveRepoData(newRepoData, repository, platformId, REPODATA);
            this.sendRepoDataEvent(
                    new CondaRepodataEvent(RepoDataEventKind.AGGREGATE, repository, platformId,
                            newRepoData)
            );
        }
    }

    private void aggregatePackage(RepoData repoData, RepoData deltaRepoData) {

        try {
            List<RepoData> repoDataList = new ArrayList<>();
            repoDataList.add(repoData);
            repoDataList.add(deltaRepoData);
            RepoData new_repoData = condaMetadataIndexer.aggregateRepoData(repoDataList);
            repoData.update(new_repoData);
        } catch (Exception e) {
            throw new RuntimeException("Failed to aggregate repo data.", e);
        }
    }

    private void saveRepoData(RepoData repoData, Repository repository, String platformId, String repoDataName) {
        RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository, platformId + "/" + repoDataName);
        try {
            Path path = Path.of(repoDataPath.toString());
            // 检查父目录是否存在，不存在则创建
            if (!Files.exists(path.getParent())) {
                Files.createDirectories(path.getParent());
            }
            Files.writeString(path, repoData.toJsonPretty());
            // 更新缓存, 映射到当前时间戳
            condaIndexCache.put(repoDataPath.toString());
        } catch (IOException e) {
            throw new RuntimeException("Failed to save RepoData to path: " + repoDataPath, e);
        }
    }


    @NonNull
    private RepoData readRepoData(Repository repository, String platformId, String repoDataName) {
        // 1. 构建索引路径
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();

        if (repository.isGroupRepository()) {
            // 组合仓库
            RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository, platformId + "/" + repoDataName);
            if (Files.exists(repoDataPath)) {
                // 1.1 如果索引文件存在，直接返回
                try (InputStream inputStream = new FileInputStream(repoDataPath.toString())) {
                    ObjectMapper objectMapper = new ObjectMapper();
                    return objectMapper.readValue(inputStream, RepoData.class);
                } catch (IOException e) {
                    throw new RuntimeException("Failed to read RepoData from path.", e);
                }
            }
            return aggregateCondaGroupPlatformRepoData(repository, platformId);
        }

        try {
            RepositoryPath repoDataPath = artifactResolutionService.resolvePath(storageId, repositoryId,
                    platformId + "/" + repoDataName);

            // 1. 检查索引是否存在, 不存在则创建新索引
            if (repoDataPath == null || !Files.exists(repoDataPath)) {
                this.reindexPackage(repository, platformId);
            }
            repoDataPath = artifactResolutionService.resolvePath(storageId, repositoryId,
                    platformId + "/" + repoDataName);

            // 2. 获取索引数据
            File repoDataFile = new File(repoDataPath.toString());
            try (InputStream inputStream = new FileInputStream(repoDataFile)) {
                ObjectMapper objectMapper = new ObjectMapper();
                RepoData repoData = objectMapper.readValue(inputStream, RepoData.class);
                if (repository.isHostedRepository() && !condaIndexCache.containsKey(repoDataPath.toString())) {
                    // 更新缓存
                    condaIndexCache.put(repoDataPath.toString());
                    this.sendRepoDataEvent(
                            new CondaRepodataEvent(RepoDataEventKind.AGGREGATE, repository, platformId,
                                    repoData)
                    );
                }
                return repoData;
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to read RepoData from path: " + repositoryId + platformId, e);
        }
    }


    @Override
    public RepoData aggregateCondaGroupPlatformRepoData(Repository repository, String platformId) {
        if (!repository.isGroupRepository()) {
            throw new IllegalArgumentException("The repository is not a group repository");
        }
        List<RepoData> repoDataList = new ArrayList<>();
        for (String id : repository.getGroupRepositories()) {
            Repository subRepository = configurationManager.getRepository(id);
            if (subRepository == null) {
                continue;
            }
            RepoData repoData = getRepoData(subRepository, platformId);
            if (repoData != null) {
                repoDataList.add(repoData);
            }
        }

        // 2. 合并索引
        try {
            RepoData groupRepoData = condaMetadataIndexer.aggregateRepoData(repoDataList);
            if (groupRepoData == null) {
                groupRepoData = condaMetadataIndexer.createNewRepoData(platformId);
            }
            saveRepoData(groupRepoData, repository, platformId, REPODATA);
            this.sendRepoDataEvent(
                    new CondaRepodataEvent(RepoDataEventKind.AGGREGATE, repository, platformId,
                            groupRepoData)
            );
            return groupRepoData;
        } catch (Exception e) {
            throw new RuntimeException("Failed to aggregate conda group platform repo data", e);
        }
    }


    @Override
    public void reindexRepository(Repository repository) {
        // 重建本地库索引
        if (!repository.isHostedRepository()) {
            return;
        }
        RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository);
        // 遍历其下所有目录(platformId)
        // 遍历其下所有目录(platformId)
        try (Stream<Path> pathStream = Files.walk(repoDataPath, 1)) {
            pathStream
                    .filter(Files::isDirectory)
                    .forEach(path -> {
                        String platformId = path.getFileName().toString();
                        // 过滤掉以.开头的目录和repoDataPath本身
                        if (!platformId.startsWith(".") && !path.equals(repoDataPath)) {
                            reindexPackage(repository, platformId);
                        }
                    });
        } catch (IOException e) {
            throw new RuntimeException("Failed to reindex repository: " + repository.getId(), e);
        }
    }
}
