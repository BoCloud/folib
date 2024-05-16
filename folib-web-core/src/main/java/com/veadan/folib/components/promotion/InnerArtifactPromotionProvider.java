package com.veadan.folib.components.promotion;

import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.dto.TargetDispatchRepositoryDto;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.enums.ArtifactSyncRecordOpsTypeEnum;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Collections;
import java.util.Date;
import java.util.List;

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
    @Inject
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;

    @PostConstruct
    @Override
    public void register() {
        artifactPromotionProviderRegistry.addProvider(ArtifactoryRepositoryTypeEnum.INNER.getType(), this);
        log.info("Registered artifact promotion '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactoryRepositoryTypeEnum.INNER.getType());
    }

    @Override
    public List<String> promotion(RepositoryPath repositoryPath, String artifactPath, UnionTargetRepositoryConfiguration unionTargetRepositoryConfiguration) {
        String storageId = repositoryPath.getStorageId();
        String repositoryId = repositoryPath.getRepositoryId();
        TargetDispatchRepositoryDto targetDispatchRepository = TargetDispatchRepositoryDto.builder().dispatchClusterEnName(unionTargetRepositoryConfiguration.getNode()).targetStorageId(unionTargetRepositoryConfiguration.getStorageId()).targetRepositoryId(unionTargetRepositoryConfiguration.getRepositoryId()).build();
        ArtifactDispatch artifactDispatch = ArtifactDispatch.builder()
                .srcStorageId(storageId)
                .srcRepositoryId(repositoryId)
                .path(artifactPath)
                .targetDispatchRepositoryList(Collections.singletonList(targetDispatchRepository))
                .recordStatus(true).build();
        log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标存储空间：{} 目标仓库：{} 满足晋级条件，开始晋级", storageId, repositoryId, artifactPath, unionTargetRepositoryConfiguration.getNode(), unionTargetRepositoryConfiguration.getType(), unionTargetRepositoryConfiguration.getStorageId(), unionTargetRepositoryConfiguration.getRepositoryId());
        return promotionUtil.executeHandleDispatch(artifactDispatch);
    }

    @Override
    public List<String> dispatch(ArtifactDispatch artifactDispatch) {
        log.info("存储空间：{} 仓库：{} 制品：{} 目标节点类型 inner 准备分发 {}", artifactDispatch.getSrcStorageId(), artifactDispatch.getSrcRepositoryId(), artifactDispatch.getPath(), JSONObject.toJSONString(artifactDispatch));
        return promotionUtil.executeHandleDispatch(artifactDispatch);
    }
}
