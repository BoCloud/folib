package com.veadan.folib.eventlistener.backup;

import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.DictService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * 事件监听，处理备份
 **/
@Slf4j
//@Component
@Deprecated
public class ArtifactEventBackupListener {

    @Inject
    private DictService dictService;

    @Inject
    private ArtifactComponent artifactComponent;

//    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        log.debug("{} 监听到制品事件：{}，path路径：{}", ArtifactEventBackupListener.class.getSimpleName(), artifactEventTypeEnum, repositoryPath);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return;
        }
        if (validateArtifactEvent(artifactEventTypeEnum) && artifactExists(repositoryPath)) {
            try {
                String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
                Dict dict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.BACKUP_SETTINGS.getType()).dictKey(String.format("%s:%s", storageId, repositoryId)).build());
                if (Objects.nonNull(dict) && StringUtils.isNotBlank(dict.getDictValue())) {
                    String backupDir = dict.getDictValue();
                    Path backupPath = Files.createDirectories(Paths.get(backupDir));
                    String sourcePath = repositoryPath.toString();
                    String prefix = String.format("/%s/%s/", storageId, repositoryId);
                    String targetSubPath = sourcePath.substring(sourcePath.indexOf(prefix) + 1);
                    Path targetPath = backupPath.resolve(targetSubPath);
                    log.info("StorageId [{}] repositoryId [{}] 开启备份功能，源制品地址 [{}] 备份制品地址 [{}]", storageId, repositoryId, sourcePath, targetPath.toString());
                    Files.createDirectories(targetPath.getParent());
                    Files.copy(repositoryPath.getTarget(), targetPath, StandardCopyOption.REPLACE_EXISTING);
                    artifactComponent.storeArtifactMetadataFile(repositoryPath, targetPath);
                }
            } catch (Exception ex) {
                log.error("事件监听，处理backup，事件类型：{} repositoryPath：{} 错误：{}", source, repositoryPath, ExceptionUtils.getStackTrace(ex));
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
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }

    /**
     * 制品存在判断
     *
     * @param repositoryPath 制品对象
     * @return true 存在 false 不存在
     */
    public boolean artifactExists(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.error("RepositoryPath [{}] does not exist", repositoryPath);
            return false;
        }
        return true;
    }

}
