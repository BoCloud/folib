package com.veadan.folib.services.Impl;

import cn.hutool.extra.spring.SpringUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.event.CondaRepodataEvent;
import com.veadan.folib.event.index.IndexEventListenerRegistry;
import com.veadan.folib.event.index.IndexTypeEnum;
import com.veadan.folib.index.indexer.CondaMetadataExtractor;
import com.veadan.folib.index.indexer.CondaMetadataIndexer;
import com.veadan.folib.index.model.RepoData;
import com.veadan.folib.index.model.RepoDataEventKind;
import com.veadan.folib.index.model.RepoDataPackage;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.CondaRepoDataService;
import com.veadan.folib.storage.repository.Repository;
import lombok.NonNull;
import net.bytebuddy.agent.builder.AgentBuilder;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.parameters.P;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * @author LingengMa
 * @date 2025/04/14 14:01
 * @Description:
 */

@Service
public class CondaRepoDataServiceImpl implements CondaRepoDataService {
    private final CondaMetadataIndexer condaMetadataIndexer;

    private final HazelcastInstance hazelcastInstance;

    private final DistributedLockComponent distributedLockComponent;

    private final ArtifactManagementService artifactManagementService;

    private final RepositoryPathResolver repositoryPathResolver;

    private final String REPODATA = "repodata.json";
    private final String CURRENT_REPODATA = "current_repodata.json";

    @Autowired
    public CondaRepoDataServiceImpl(CondaMetadataIndexer condaMetadataIndexer, HazelcastInstance hazelcastInstance, DistributedLockComponent distributedLockComponent, ArtifactManagementService artifactManagementService, RepositoryPathResolver repositoryPathResolver) {
        this.condaMetadataIndexer = condaMetadataIndexer;
        this.hazelcastInstance = hazelcastInstance;
        this.distributedLockComponent = distributedLockComponent;
        this.artifactManagementService = artifactManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
    }

    @Override
    public RepoData getRepoData(Repository repository, String platformId) {
        return readRepoData(repository, platformId, REPODATA);
    }

    @Override
    public RepoData getCurrentRepoData(Repository repository, String platformId) {
        return readRepoData(repository, platformId, CURRENT_REPODATA);
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

        RepoData repoData = getRepoData(repository, platformId);
        RepoData currentRepoData = getCurrentRepoData(repository, platformId);

        if (kind == RepoDataEventKind.ADD) {
            addPackage(repoData, currentRepoData, artifactName, event.getRepoDataPackage());
        } else if (kind == RepoDataEventKind.REMOVE) {
            removePackage(repoData, currentRepoData, platformId, artifactName);
        } else if (kind == RepoDataEventKind.REINDEX) {
            reindexPackage(repoData, repository, platformId);
        } else if (kind == RepoDataEventKind.AGGREGATE) {
            aggregatePackage(repoData, event.getRepoData());
        }
        else {
            throw new RuntimeException("Unsupported RepoDataEventKind: " + kind);
        }
        saveRepoData(repoData, repository, platformId, REPODATA);
        saveRepoData(currentRepoData, repository, platformId, CURRENT_REPODATA);
        // 组仓库更新
        if (kind == RepoDataEventKind.ADD || kind == RepoDataEventKind.REINDEX || kind == RepoDataEventKind.AGGREGATE) {
            String storageId = repository.getStorage().getId();
            String repositoryId = repository.getId();
            //发送索引更新事件
            IndexEventListenerRegistry registry = SpringUtil.getBean(IndexEventListenerRegistry.class);
            registry.dispatchUpdateIndexEvent(storageId, repositoryId, IndexTypeEnum.CONDA);
        }
    }

    @Override
    public boolean checkPackageExistsInRepoData(RepoData repoData,
                                                String artifactName) {
        return condaMetadataIndexer.checkPackageExistsInRepoData(repoData, artifactName);
    }

    private void addPackage(RepoData repoData, RepoData currentRepoData, String artifactName,
                            RepoDataPackage repoDataPackage) {
        if (this.checkPackageExistsInRepoData(repoData, artifactName)) {
            return;
        }

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

    private void reindexPackage(RepoData repoData, Repository repository, String platformId) {
        // 1. 获取索引数据
        RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository, platformId);
        String repoKey = repoDataPath.toString();
        RepoData newRepoData = condaMetadataIndexer.reindexRepoData(repoKey);
        if (newRepoData == null) {
            throw new RuntimeException("Failed to reindex repo data for repoKey.");
        }
        // 2. 更新索引数据
        repoData.update(newRepoData);

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
            hazelcastInstance.getMap("condaRepoData").put(repoDataPath.toString(), new Date());
        } catch (IOException e) {
            throw new RuntimeException("Failed to save RepoData to path: " + repoDataPath, e);
        }
    }


    @NonNull
    private RepoData readRepoData(Repository repository, String platformId, String repoDataName) {
        // 1. 构建索引路径
        RepositoryPath repoDataPath = repositoryPathResolver.resolve(repository, platformId + "/" + repoDataName);

        // 1. 检查repoDataPath是否存在
        if (repoDataPath == null || !Files.exists(repoDataPath)) {
            return condaMetadataIndexer.createNewRepoData(platformId);
        }

        // 2. 获取索引数据
        try (InputStream inputStream = new FileInputStream(repoDataPath.toFile())) {
            ObjectMapper objectMapper = new ObjectMapper();
            return objectMapper.readValue(inputStream, RepoData.class);
        } catch (IOException e) {
            throw new RuntimeException("Failed to read RepoData from path: " + repoDataPath, e);
        }
    }

}
