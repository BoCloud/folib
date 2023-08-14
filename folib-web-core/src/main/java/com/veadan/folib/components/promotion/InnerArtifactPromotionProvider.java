package com.veadan.folib.components.promotion;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.dto.TargetDispatchRepositoryDto;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Collections;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class InnerArtifactPromotionProvider implements ArtifactPromotionProvider {

    @Inject
    private ArtifactPromotionProviderRegistry artifactPromotionProviderRegistry;

    @Inject
    private PromotionUtil promotionUtil;

    @PostConstruct
    @Override
    public void register() {
        artifactPromotionProviderRegistry.addProvider(ArtifactoryRepositoryTypeEnum.INNER.getType(), this);
        log.info("Registered artifact promotion '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactoryRepositoryTypeEnum.INNER.getType());
    }

    @Override
    public void promotion(RepositoryPath repositoryPath, String artifactPath, UnionTargetRepositoryConfiguration unionTargetRepositoryConfiguration) {
        String storageId = repositoryPath.getStorageId();
        String repositoryId = repositoryPath.getRepositoryId();
        TargetDispatchRepositoryDto targetDispatchRepository = TargetDispatchRepositoryDto.builder().dispatchClusterEnName(unionTargetRepositoryConfiguration.getNode()).targetStorageId(unionTargetRepositoryConfiguration.getStorageId()).targetRepositoryId(unionTargetRepositoryConfiguration.getRepositoryId()).build();
        ArtifactDispatch artifactDispatch = ArtifactDispatch.builder().srcStorageId(storageId).srcRepositoryId(repositoryId).path(artifactPath)
                .targetDispatchRepositoryList(Collections.singletonList(targetDispatchRepository)).recordStatus(true).build();
        log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标存储空间：{} 目标仓库：{} 满足晋级条件，开始晋级", storageId, repositoryId, artifactPath, unionTargetRepositoryConfiguration.getNode(), unionTargetRepositoryConfiguration.getType(), unionTargetRepositoryConfiguration.getStorageId(), unionTargetRepositoryConfiguration.getRepositoryId());
        promotionUtil.executeHandleDispatch(artifactDispatch);
    }

    @Override
    public void dispatch(ArtifactDispatch artifactDispatch) {
        log.info("存储空间：{} 仓库：{} 制品：{} 目标节点类型 inner 准备分发 {}", artifactDispatch.getSrcStorageId(), artifactDispatch.getSrcRepositoryId(), artifactDispatch.getPath(), JSONObject.toJSONString(artifactDispatch));
        promotionUtil.executeHandleDispatch(artifactDispatch);
    }
}
