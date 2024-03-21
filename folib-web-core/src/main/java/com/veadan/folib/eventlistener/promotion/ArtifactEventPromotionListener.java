package com.veadan.folib.eventlistener.promotion;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.promotion.ArtifactPromotionProvider;
import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.configuration.UnionRepositoryConfiguration;
import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.enums.UnionRepositorySyncTypeEnum;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * @author leipenghui
 * 事件监听，处理制品自动晋级
 **/
@Slf4j
@Component
public class ArtifactEventPromotionListener {

    @Inject
    private ArtifactPromotionProviderRegistry artifactPromotionProviderRegistry;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private ScanRulesMapper scanRulesMapper;

    @EventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        log.debug("监听到制品事件 [{}]，path路径 [{}]", artifactEventTypeEnum, repositoryPath);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return;
        }
        if (validateArtifactEvent(artifactEventTypeEnum) && artifactComponent.layoutSupports(repositoryPath)) {
            try {
                Repository repository = artifactComponent.getRepository(repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
                if (Objects.isNull(repository)) {
                    log.debug("仓库 [{}] 不存在，无后续操作", RepositoryFiles.relativizePath(repositoryPath));
                    return;
                }
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (Objects.isNull(artifact)) {
                    log.debug("制品 [{}] 不存在，无后续操作", RepositoryFiles.relativizePath(repositoryPath));
                    return;
                }
                String storageId = repository.getStorage().getId();
                String repositoryId = repository.getId();
                UnionRepositoryConfiguration unionRepositoryConfiguration = repository.getUnionRepositoryConfig();
                if (Objects.isNull(unionRepositoryConfiguration)) {
                    log.debug("存储空间 [{}] 仓库 [{}] 未设置联邦仓库，无后续操作", storageId, repositoryId);
                    return;
                }
                if (Boolean.FALSE.equals(unionRepositoryConfiguration.getEnable())) {
                    log.debug("存储空间 [{}] 仓库 [{}] 晋级未启用，无后续操作", storageId, repositoryId);
                    return;
                }
                log.info("存储空间 [{}] 仓库 [{}] 联邦仓库信息 [{}]", storageId, repositoryId, JSONObject.toJSONString(unionRepositoryConfiguration));
                Set<UnionTargetRepositoryConfiguration> unionTargetRepositoryConfigurations = unionRepositoryConfiguration.getUnionTargetRepositories();
                if (CollectionUtils.isEmpty(unionTargetRepositoryConfigurations)) {
                    log.debug("存储空间 [{}] 仓库 [{}] 未设置联邦目标仓库，无后续操作", storageId, repositoryId);
                    return;
                }
                String artifactPath = getArtifactPath(repositoryPath, artifact);
                Integer syncType = unionRepositoryConfiguration.getSyncType();
                boolean promotionFlag = false;
                if (UnionRepositorySyncTypeEnum.ARTIFACT_PATH.getType().equals(syncType)) {
                    //制品路径
                    Set<String> artifactPaths = unionRepositoryConfiguration.getArtifactPaths();
                    log.debug("存储空间 [{}] 仓库 [{}] 制品配置路径 [{}]，制品路径 [{}]", storageId, repositoryId, artifactPaths, artifactPath);
                    promotionFlag = artifactPaths.stream().allMatch("*"::equals) || artifactPaths.stream().anyMatch(artifactPath::contains);
                    if (!promotionFlag) {
                        //使用正则再匹配一次
                        promotionFlag = artifactPaths.stream().anyMatch(regex -> Pattern.matches(regex, artifactPath));
                    }
                } else if (UnionRepositorySyncTypeEnum.METADATA.getType().equals(syncType)) {
                    //元数据
                    JSONObject metadataJson = artifactComponent.getMetadata(artifact);
                    if (Objects.isNull(metadataJson)) {
                        log.debug("存储空间 [{}] 仓库 [{}] 制品 [{}] 未找到元数据，无后续操作", storageId, repositoryId, artifactPath);
                        return;
                    }
                    String metadataKey = unionRepositoryConfiguration.getMetadataKey();
                    String metadataValue = unionRepositoryConfiguration.getMetadataValue();
                    String valueKey = "value";
                    log.debug("存储空间 [{}] 仓库 [{}] 制品 [{}] 配置元数据key [{}] 配置元数据value [{}] 元数据 [{}]", storageId, repositoryId, artifactPath, metadataKey, metadataValue, metadataJson);
                    promotionFlag = metadataJson.containsKey(metadataKey) && metadataJson.getJSONObject(metadataKey).get(valueKey).equals(metadataValue);
                }
                if (promotionFlag) {
                    boolean promotionBlock = artifactComponent.promotionBlock();
                    Example example = new Example(ScanRules.class);
                    example.createCriteria().andEqualTo("onScan", 1).andEqualTo("storage", storageId)
                            .andEqualTo("repository", repositoryId);
                    List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
                    boolean scanEnable = CollectionUtils.isNotEmpty(scanRulesList);
                    log.debug("自动晋级阻断开关状态 [{}]，仓库扫描状态 [{}]", promotionBlock, scanEnable);
                    if (promotionBlock && scanEnable) {
                        //加入晋级
                        log.debug("存储空间 [{}] 仓库 [{}] 制品 [{}] 满足初步晋级条件，晋级状态为待晋级", storageId, repositoryId, artifactPath);
                        for (UnionTargetRepositoryConfiguration unionTargetRepository : unionTargetRepositoryConfigurations) {
                            artifactComponent.handlerArtifactPromotion(unionTargetRepository.getNode(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.WAIT.getStatus());
                        }
                    } else {
                        //开始晋级
                        for (UnionTargetRepositoryConfiguration unionTargetRepository : unionTargetRepositoryConfigurations) {
                            try {
                                ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(unionTargetRepository.getType());
                                artifactPromotionProvider.promotion(repositoryPath, artifact.getArtifactPath(), unionTargetRepository);
                            } catch (Exception ex) {
                                log.error("存储空间 [{}] 仓库 [{}] 处理自动晋级，repositoryPath [{}] 联邦仓库 [{}] 错误 [{}]", storageId, repositoryId, repositoryPath, JSONObject.toJSONString(unionTargetRepository), ExceptionUtils.getStackTrace(ex));
                                artifactComponent.handlerArtifactPromotion(unionTargetRepository.getNode(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.FAIL.getStatus());
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                log.error("事件监听，处理自动晋级，事件类型 [{}] repositoryPath [{}] 错误 [{}]", source, repositoryPath, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean validateArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }

    /**
     * 获取制品路径
     *
     * @param repositoryPath repositoryPath
     * @param artifact       artifact
     * @return 制品路径
     */
    private String getArtifactPath(RepositoryPath repositoryPath, Artifact artifact) {
        String artifactPath = artifact.getArtifactPath();
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            String path = artifact.getArtifactPath();
            artifactPath = path.substring(0, path.indexOf("/sha256"));
        }
        return artifactPath;
    }

}
