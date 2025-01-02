package com.veadan.folib.components.syncartifact;

import com.google.common.collect.Lists;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.DistributedCounterComponent;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.entity.MigrateInfo;
import com.veadan.folib.enums.ArtifactSyncTypeEnum;
import com.veadan.folib.enums.MigrateStatusEnum;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.indexer.HelmMetadataIndexer;
import com.veadan.folib.model.HelmChartMetadata;
import com.veadan.folib.model.HelmIndexYamlMetadata;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.JfrogMigrateService;
import com.veadan.folib.services.MigrateInfoService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.SortedSet;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class HelmSyncArtifactProvider implements SyncArtifactProvider {

    /**
     * 计数
     */
    private static final AtomicLong COUNT = new AtomicLong(0);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private CommonComponent commonComponent;


    @Resource
    private ClusterSyncService clusterSyncService;

    @Resource
    private DistributedCounterComponent distributedCounterComponent;

    @Resource
    private DistributedCacheComponent distributedCacheComponent;
    @Resource
    private ConfigurationManagementService configurationManagementService;

    @Resource
    private MigrateInfoService migrateInfoService;

    @PostConstruct
    @Override
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.HELM.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.HELM.getType());
    }

    @Override
    public void browseFullSync(SyncArtifactForm syncArtifactForm) {
        RepositoryPath indexRepositoryPath = syncPackageIndex(syncArtifactForm);
        if (Objects.isNull(indexRepositoryPath) || !Files.exists(indexRepositoryPath)) {
            log.error("Helm包索引不存在 存储空间 [{}] 仓库 [{}]", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            return;
        }
        handlerIndex(indexRepositoryPath, syncArtifactForm);
    }

    @Override
    public void fullSync(SyncArtifactForm syncArtifactForm) {

    }

    private RepositoryPath syncPackageIndex(SyncArtifactForm syncArtifactForm) {
        try {
            long startTime = System.currentTimeMillis();
            Repository repository = configurationManager.getRepository(syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            if (Objects.isNull(repository)) {
                throw new RuntimeException(String.format("存储空间 [%s] 所属仓库 [%s}] 不存在", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId()));
            }
            if (!RepositoryTypeEnum.PROXY.getType().equalsIgnoreCase(repository.getType())) {
                throw new RuntimeException(String.format("存储空间 [%s] 所属仓库 [%s}] 不是代理库", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId()));
            }
            String storageId = syncArtifactForm.getStorageId(), repositoryId = syncArtifactForm.getRepositoryId();
            RepositoryPath indexRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "index.yaml");
            if (StringUtils.isNotBlank(syncArtifactForm.getIndexPath())) {
                indexRepositoryPath.setTargetUrl(syncArtifactForm.getIndexPath());
            }
            artifactResolutionService.resolvePath(indexRepositoryPath);
            log.info("Helm包索引同步完成，存储空间 [{}] 仓库 [{}] 耗时 [{}] ms", storageId, repositoryId, System.currentTimeMillis() - startTime);
            return indexRepositoryPath;
        } catch (Exception e) {
            log.error("Helm包索引同步错误 [{}]", ExceptionUtils.getStackTrace(e));
        }
        return null;
    }

    private void handlerIndex(RepositoryPath repositoryPath, SyncArtifactForm syncArtifactForm) {
        long startTime = System.currentTimeMillis();
        String storageId = syncArtifactForm.getStorageId(), repositoryId = syncArtifactForm.getRepositoryId();
        HelmMetadataIndexer helmMetadataIndexer = new HelmMetadataIndexer(storageId, repositoryId, null, null);
        HelmIndexYamlMetadata helmIndexYamlMetadata = helmMetadataIndexer.readFromIndexYaml(repositoryPath);
        if (Objects.isNull(helmIndexYamlMetadata)) {
            return;
        }
        ConcurrentMap<String, SortedSet<HelmChartMetadata>> entriesMap = helmIndexYamlMetadata.entries;
        if (MapUtils.isEmpty(entriesMap)) {
            return;
        }
        Repository repository = repositoryPath.getRepository();
        String url = repository.getRemoteRepository().getUrl();
        COUNT.set(0L);
        int availableCores = commonComponent.getAvailableCores() * 2;
        ThreadPoolTaskExecutor threadPoolTaskExecutor = commonComponent.buildThreadPoolTaskExecutor("browseHelmSync", availableCores, availableCores);
        int batch = 20;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        List<String> artifactPathList = Lists.newArrayList(), itemArtifactPathList;
        for (Map.Entry<String, SortedSet<HelmChartMetadata>> entry : entriesMap.entrySet()) {
            if("0".equals(distributedCacheComponent.get(JfrogMigrateService.PAUSED_FLAG_PRE+syncArtifactForm.getStoreAndRepo()))){
                //
                log.info("仓库{}同步任务暂停",syncArtifactForm.getStoreAndRepo());
                migrateInfoService.updateAndSyncRepoStatus(syncArtifactForm,MigrateStatusEnum.PAUSED.getStatus());
                distributedCacheComponent.delete(JfrogMigrateService.PAUSED_FLAG_PRE+syncArtifactForm.getStoreAndRepo());
                return;
            }
            SortedSet<HelmChartMetadata> helmChartMetadataSortedSet = entry.getValue();
            if (CollectionUtils.isEmpty(helmChartMetadataSortedSet)) {
                continue;
            }
            for (HelmChartMetadata helmChartMetadata : helmChartMetadataSortedSet) {
                if (CollectionUtils.isEmpty(helmChartMetadata.urls)) {
                    continue;
                }
                itemArtifactPathList = helmChartMetadata.urls.stream().filter(StringUtils::isNotBlank).map(item -> StringUtils.removeStart(item.replace(url, ""), GlobalConstants.SEPARATOR)).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(itemArtifactPathList)) {
                    continue;
                }
                artifactPathList.addAll(itemArtifactPathList);
            }
            if (artifactPathList.size() >= batch) {
                batchDownload(storageId, repositoryId, artifactPathList, threadPoolTaskExecutor);
            }
        }
        if (CollectionUtils.isNotEmpty(artifactPathList)) {
            batchDownload(storageId, repositoryId, artifactPathList, threadPoolTaskExecutor);
        }
        log.info("Helm包同步完成，存储空间 [{}] 仓库 [{}] 同步 [{}] 个制品，耗时 [{}] ms", storageId, repositoryId, COUNT.get(), System.currentTimeMillis() - startTime);
    }

    private void batchDownload(String storageId, String repositoryId, List<String> artifactPathList, ThreadPoolTaskExecutor threadPoolTaskExecutor) {
        if (CollectionUtils.isEmpty(artifactPathList)) {
            return;
        }
        List<List<String>> artifactPathLists = Lists.partition(artifactPathList, 2);
        List<FutureTask<String>> futureTasks = Lists.newArrayList();
        FutureTask<String> futureTask = null;
        for (List<String> itemArtifactPathList : artifactPathLists) {
            futureTask = new FutureTask<String>(() -> {
                for (String artifactPath : itemArtifactPathList) {
                    try {
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                        if (Files.exists(repositoryPath)) {
                            COUNT.incrementAndGet();
                            log.debug("Batch download storageId [{}] repositoryId [{}] artifactPath [{}] exists skip..", storageId, repositoryId, artifactPath);
                            continue;
                        }
                        artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
                        if (Files.exists(repositoryPath)) {
                            COUNT.incrementAndGet();
                            distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT+storageId+":"+repositoryId).addAndGet(1L);
                        }
                    } catch (Exception ex) {
                        log.error("Batch download storageId [{}] repositoryId [{}] artifactPath [{}] error [{}]", storageId, repositoryId, artifactPath, ExceptionUtils.getStackTrace(ex));
                    }
                }
                return "success";
            });
            futureTasks.add(futureTask);
            threadPoolTaskExecutor.submit(futureTask);
        }
        futureTasks.forEach(action -> {
            try {
                action.get();
            } catch (Exception e) {
                log.error(e.getMessage(), e);
            }
        });
        //清理
        artifactPathList.clear();
    }



    @Override
    public void batchBrowseSync(SyncArtifactForm syncArtifactForm) {
        // 获取仓库信息
        MigrateInfo repository = migrateInfoService.getByMigrateIdAndRepoInfo(syncArtifactForm.getMigrateId(),syncArtifactForm.getStorageId(),syncArtifactForm.getRepositoryId());
        if(MigrateStatusEnum.QUEUING.getStatus()==repository.getSyncStatus()){
            syncPackageIndex(syncArtifactForm);
            repository.setTotalArtifact(syncArtifactForm.getTotalArtifact());
            repository.setSyncStatus(MigrateStatusEnum.SYNCING_ARTIFACT.getStatus());
            // 更新状态
            migrateInfoService.updateById(repository);
        }
        RepositoryPath indexRepositoryPath = repositoryPathResolver.resolve(syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), "index.yaml");
        distributedCacheComponent.put(JfrogMigrateService.PAUSED_FLAG_PRE+syncArtifactForm.getStoreAndRepo(),"1");
        handlerIndex(indexRepositoryPath, syncArtifactForm);

    }
}
