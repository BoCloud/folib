package com.veadan.folib.components.syncartifact;

import com.veadan.folib.components.DistributedCounterComponent;
import com.veadan.folib.components.files.FilesCommonComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.migrate.SyncArtifactForm;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Objects;

/**
 * @author huayanjun
 * @since 2025-01-20 16:54
 */

@Slf4j
@Component
public class SyncUtils {

    final String ARTIFACT_COUNT = "migrate:artifact:count:";
    final String INDEX_COUNT = "migrate:index:count:";

    @Resource
    private DistributedCounterComponent distributedCounterComponent;

    @Resource
    private ConfigurationManager configurationManager;

    @Resource
    private FilesCommonComponent filesCommonComponent;

    public void resetIndex(String storeAndRepo){
        distributedCounterComponent.getAtomicLong(INDEX_COUNT + storeAndRepo).set(0L);
    }

    public void resetArtifact(String storeAndRepo){
        distributedCounterComponent.getAtomicLong(ARTIFACT_COUNT + storeAndRepo).set(0L);

    }

    public void indexIncrease(String storeAndRepo) {
        distributedCounterComponent.getAtomicLong(INDEX_COUNT + storeAndRepo).getAndAdd(1);
    }

    public int getIndexCount(String storeAndRepo) {
        return (int) distributedCounterComponent.getAtomicLong(INDEX_COUNT + storeAndRepo).get();
    }

    public void artifactIncrease(String storeAndRepo) {
        distributedCounterComponent.getAtomicLong(ARTIFACT_COUNT + storeAndRepo).getAndAdd(1);
    }

    public int getArtifactCount(String storeAndRepo) {
        return (int) distributedCounterComponent.getAtomicLong(ARTIFACT_COUNT + storeAndRepo).get();
    }

    public Repository validRepo(SyncArtifactForm syncArtifactForm){
        Repository repository = configurationManager.getRepository(syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
        if (Objects.isNull(repository)) {
            log.error("存储空间 【{}】 所属仓库 【{}】 仓库不存在",syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            return null;
        }
        if (!RepositoryTypeEnum.PROXY.getType().equalsIgnoreCase(repository.getType())) {
            log.error("存储空间【{}】 所属仓库 【{}】 不是代理库", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            return null;
        }
        return repository;
    }


    public String getBaseUri(){
        return configurationManager.getBaseUri().toString();
    }

    public void storeContent(String absUrl, String path){
        filesCommonComponent.storeContent(absUrl,path);
    }
}
