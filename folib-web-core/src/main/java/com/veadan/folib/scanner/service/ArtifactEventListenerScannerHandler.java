package com.veadan.folib.scanner.service;

import com.veadan.folib.artifact.archive.JarArchiveListingFunction;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.*;
import com.veadan.folib.scanner.common.constant.ScanConstans;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * 制品删除事件监听处理
 */
@Slf4j
@Component
public class ArtifactEventListenerScannerHandler {

    @Autowired
    private ScanService scanService;

    @AsyncEventListener
    protected void handle(final ArtifactEvent<RepositoryPath> event) {
        RepositoryPath repositoryPath = event.getPath();
        int source = (int) event.getSource();
        boolean flag = check(event);
        if (!flag) {
            return;
        }
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() == source) {
            scanService.checkScan(repositoryPath, ScanConstans.DEL);
        } else if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType() == source) {
            scanService.checkScan(repositoryPath, ScanConstans.ADD);
        } else if (ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() == source) {
            scanService.checkScan(repositoryPath, ScanConstans.DEL_DIRECTORY);
        } else {
            scanService.checkScan(repositoryPath, null);
        }
    }

    /**
     * 入参校验
     *
     * @param event 制品事件
     * @return true 校验通过 false 校验不通过
     */
    private boolean check(ArtifactEvent<RepositoryPath> event) {
        boolean flag = false;
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        log.info("=====>>>>> 监听制品事件：{}，path路径：{}", ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source), repositoryPath);
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return false;
        }
        flag = checkArtifactEvent(artifactEventTypeEnum);
        log.info("=====>>>>> 制品事件类型是否为需要处理的类型：{}", flag);
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() == source) {
            //删除制品目录后续不需要校验文件类型是否支持
            return true;
        }
        if (flag) {
            flag = checkSupports(repositoryPath);
        }
        return flag;
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean checkArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }

    /**
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    private boolean checkSupports(RepositoryPath repositoryPath) {
        boolean flag = false;
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            log.info("=====>>>>> docker布局");
            //docker布局 TODO
        } else if (repositoryPath.getFileSystem() instanceof MavenFileSystem) {
            log.info("=====>>>>> maven布局");
            //maven布局
            flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath);
        } else if (repositoryPath.getFileSystem() instanceof NpmFileSystem) {
            log.info("=====>>>>> npm布局");
            //npm布局
            List<String> suffixList = Arrays.asList(".json", ".js", ".tgz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof NugetFileSystem) {
            log.info("=====>>>>> nuget布局");
            //nuget布局 TODO
        } else if (repositoryPath.getFileSystem() instanceof PypiFileSystem) {
            log.info("=====>>>>> pypi布局");
            //pypi布局
            List<String> suffixList = Arrays.asList(".whl", ".egg", ".zip", ".gz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof RawFileSystem) {
            log.info("=====>>>>> raw布局");
            //raw布局
            return true;
        }
        log.info("=====>>>>> 是否是该布局支持的类型：{}", flag);
        return flag;
    }

    /**
     * 判断路径是否以某种后缀结尾
     *
     * @param path       路径
     * @param suffixList 后缀列表
     * @return true
     */
    private boolean endsWith(String path, List<String> suffixList) {
        boolean flag = false;
        if (StringUtils.isNotBlank(path) && CollectionUtils.isNotEmpty(suffixList)) {
            for (String item : suffixList) {
                if (path.endsWith(item)) {
                    flag = true;
                    break;
                }
            }
        }
        return flag;
    }

}
