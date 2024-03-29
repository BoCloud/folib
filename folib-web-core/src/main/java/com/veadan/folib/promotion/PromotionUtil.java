package com.veadan.folib.promotion;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.*;
import com.veadan.folib.dto.*;
import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.forms.common.StorageTreeForm;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.RepositoryPathUtil;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.client.handler.command.FolibWsClientArtifactPullCommand;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.glassfish.jersey.media.multipart.Boundary;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.file.StreamDataBodyPart;
import org.glassfish.jersey.media.multipart.internal.MultiPartWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.FutureTask;
import java.util.stream.Collectors;

/**
 * @author qijianping
 */
@Component
@Slf4j
public class PromotionUtil {

    private final String upLoadURI = "/api/artifact/folib/promotion/upload-files";

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    protected ConfigurationManagementService configurationManagementService;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Autowired
    private ThreadPoolTaskExecutor asyncCopyThreadPoolTaskExecutor;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private ArtifactService artifactService;

    @Autowired
    private ArtifactWebService artifactWebService;

    @Autowired
    private SecurityComponent securityComponent;

    @Autowired
    @Lazy
    private ArtifactComponent artifactComponent;

    @Autowired
    @Lazy
    private DockerComponent dockerComponent;

    @Async("asyncCopyThreadPoolTaskExecutor")
    public void executeCopy(RepositoryPath path, Repository srcRepository, Repository targetRepository) {
        try {
            handleCopy(path, srcRepository, targetRepository);
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path);
        } catch (Exception e) {
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path, ExceptionUtils.getStackTrace(e));
        }
    }

    @Async("asyncPromotionPoolTaskExecutor")
    public void executeHandleDispatch(ArtifactDispatch artifactDispatch) {
        // 获取分发配置信息
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        if (MapUtil.isEmpty(map)) {
            log.error("分发配置为空!");
            return;
        }
        List<TargetDispatchRepositoryDto> targetRepositoryList = artifactDispatch.getTargetDispatchRepositoryList();
        String artifactPath = artifactDispatch.getPath();
        if (StringUtils.isBlank(artifactPath)) {
            log.warn("分发 path 不为空");
            return;
        }
        for (TargetDispatchRepositoryDto targetDispatchRepositoryDto : targetRepositoryList) {
            // 异步并发处理
            handlerDispatch(map, artifactDispatch, targetDispatchRepositoryDto);
        }
    }

    public void handlerDispatch(Map<String, ClusterDispatchNodeDto> map, ArtifactDispatch artifactDispatch,
                                TargetDispatchRepositoryDto targetDispatchRepositoryDto) {
        Response response = null;
        try {
            String artifactPath = artifactDispatch.getPath();
            String srcRepositoryId = artifactDispatch.getSrcRepositoryId();
            String srcStorageId = artifactDispatch.getSrcStorageId();
            String dispatchClusterName = targetDispatchRepositoryDto.getDispatchClusterEnName();
            String targetStorageId = targetDispatchRepositoryDto.getTargetStorageId();
            String targetRepositoryId = targetDispatchRepositoryDto.getTargetRepositoryId();
            String type = artifactDispatch.getType();
            String layout = artifactDispatch.getLayout();
            String policy = artifactDispatch.getPolicy();
            Boolean recordStatus = artifactDispatch.getRecordStatus();

            // 因三级联动插件原因 全选一级 或者二级 会得到 存储空间或者仓库为空的情况 ，如果仓库为空则需要再查一遍分发集群仓库信息。
            if (StringUtils.isBlank(dispatchClusterName)) {
                log.error("分发集群名不为空!");
                return;
            }
            ClusterDispatchNodeDto dispatchNodeDto = map.get(dispatchClusterName);
            if (null == dispatchNodeDto) {
                log.error("{} 分发配置不存在", dispatchClusterName);
                return;
            }
            if (StringUtils.isBlank(targetStorageId) || StringUtils.isBlank(targetRepositoryId)) {
                Map<String, ClusterDispatchNodeDto> dispatchMap = configurationManagementService.
                        getMutableConfigurationClone().getClusterDispatchNode();
                Client client = clientPool.getRestClient();
                ClusterDispatchNodeDto clusterDispatchNodeDto = dispatchMap.get(dispatchClusterName);
                ArtifactDispatchRepositoryDto dispatchRepositoryDto = ArtifactDispatchRepositoryDto.builder()
                        .type(type)
                        .layout(layout)
                        .dispatchEnName(dispatchClusterName)
                        .policy(policy).build();

                String host = clusterDispatchNodeDto.getClusterNodeHost();
                String url = host.endsWith("/") ? host + "api/configuration/folib/storages/getDispatchRepositories" :
                        host + "/api/configuration/folib/storages/getDispatchRepositories";
                WebTarget target = client.target(url);
                log.info(" 请求分发获取仓库信息 {}", JSONUtil.toJsonStr(dispatchRepositoryDto));
                Invocation.Builder builder = target.request();
                securityComponent.securityTokenHeader(builder);
                response = builder.post(Entity.entity(dispatchRepositoryDto, javax.ws.rs.core.MediaType.APPLICATION_JSON));
                if (response.getStatus() != 200) {
                    log.error("dispatch cluster {} get repositroy fail", dispatchClusterName);
                }
                DispatchStorageTree dispatchStorageTree = response.readEntity(DispatchStorageTree.class);
                List<StorageTreeForm> storageTreeForms = dispatchStorageTree.getList();
                if (StringUtils.isBlank(targetStorageId)) {
                    // 查询集群下全部的仓库（同类型 同策略 同布局）
                    for (StorageTreeForm storageTreeForm : storageTreeForms) {
                        List<StorageTreeForm> storages = storageTreeForm.getChildren();
                        if (CollectionUtil.isEmpty(storages)) {
                            continue;
                        }
                        for (StorageTreeForm storage : storages) {
                            targetStorageId = storage.getName();
                            List<StorageTreeForm> repos = storage.getChildren();
                            if (CollectionUtil.isEmpty(repos)) {
                                continue;
                            }
                            for (StorageTreeForm repo : repos) {
                                String tempRepoId = repo.getName();
                                executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, tempRepoId, dispatchNodeDto, recordStatus);
                            }
                        }
                    }
                }
                if (StringUtils.isBlank(targetRepositoryId)) {
                    // 选存储空间下的全部仓库（同类型 同策略 同布局）
                    for (StorageTreeForm storageTreeForm : storageTreeForms) {
                        List<StorageTreeForm> storages = storageTreeForm.getChildren();
                        if (CollectionUtil.isEmpty(storages)) {
                            continue;
                        }
                        for (StorageTreeForm storage : storages) {
                            String tempStorage = storage.getName();
                            if (!tempStorage.equals(targetStorageId)) {
                                continue;
                            }
                            List<StorageTreeForm> repos = storage.getChildren();
                            if (CollectionUtil.isEmpty(repos)) {
                                continue;
                            }
                            for (StorageTreeForm repo : repos) {
                                String tempRepoId = repo.getName();
                                executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, tempRepoId, dispatchNodeDto, recordStatus);
                            }
                            break;
                        }
                    }
                }
            } else {
                executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, targetRepositoryId, dispatchNodeDto, recordStatus);
            }
        } catch (Exception e) {
            log.error("分发错误： {}", ExceptionUtils.getStackTrace(e));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    private void executeDispatch(String artifactPath, String srcRepositoryId, String srcStorageId, String targetStorageId, String targetRepositoryId, ClusterDispatchNodeDto dispatchNodeDto, Boolean recordStatus) {
        Response response = null;
        try {
            StringBuilder strBuilder = new StringBuilder();
            String dispatchNodeHost = dispatchNodeDto.getClusterNodeHost();
            strBuilder.append(dispatchNodeHost);
            if (dispatchNodeHost.endsWith("/")) {
                strBuilder.append(targetStorageId);
            } else {
                strBuilder.append("/");
                strBuilder.append(targetStorageId);
            }
            strBuilder.append("/").append(targetRepositoryId).append("/").append(artifactPath);
            String targetPath = strBuilder.toString();
            String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();
            String sourcePath = baseUrl.endsWith("/") ? baseUrl + srcStorageId + "/" + srcRepositoryId + "/" + artifactPath :
                    baseUrl + "/" + srcStorageId + "/" + srcRepositoryId + "/" + artifactPath;
            String dispatchType = dispatchNodeDto.getDispatchType();
            PromotionNodeOption promotionNodeOption = null;
            log.info("分发 [{}] 开始", dispatchType);
            if (dispatchType.equals("pull")) {
                promotionNodeOption = new PromotionNodeOption(sourcePath, targetPath);
                promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PULL.getVal());

                // 通过Ws协议通知客户端拉取制品
                final String clusterNodeHost = dispatchNodeDto.getClusterNodeHost();
                final String nodeHost = UrlUtils.getHost(clusterNodeHost);
                final Integer nodePort = UrlUtils.getPort(clusterNodeHost);
                final String nodeName = String.format("%s:%s", nodeHost, nodePort);
                final FolibWsServerRunManage.FolibWsClientRun wsClientRun = FolibWsServerRunManage.getWsClientRun(nodeName);
                if (null == wsClientRun) {
                    promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
                    dispatchNodeDto.setDispatchType("push");
                    this.executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, targetRepositoryId, dispatchNodeDto, recordStatus);
                    return;
                }

                final FolibWsAction folibWsAction = new FolibWsAction()
                        .command(FolibWsClientArtifactPullCommand.COMMAND)
                        .payload(promotionNodeOption);
                wsClientRun.doAction(folibWsAction);
            } else {
                Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPath);
                //  遍历所有制品文件后逐步上传
                String srcAbsolutePath = srcPath.getTarget().toString();
                String targetUploadUrl = dispatchNodeHost.endsWith("/")
                        ? dispatchNodeHost + "api/artifact/folib/promotion/upload-files" :
                        dispatchNodeHost + "/api/artifact/folib/promotion/upload-files";
                PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(srcStorageId, srcRepositoryId,
                        targetStorageId, targetRepositoryId, srcAbsolutePath, targetUploadUrl);
                PromotionNodeOptionDto uploadDto = getPromotionUploadDto(promotionArtifactDto);
                upload(targetUploadUrl, uploadDto);
                if (Boolean.TRUE.equals(recordStatus)) {
                    artifactComponent.handlerArtifactPromotion(dispatchNodeDto.getClusterEnName(), srcStorageId, srcRepositoryId, artifactPath, PromotionStatusEnum.SUCCESS.getStatus());
                }
            }
            log.info("分发 [{} {} {} {} {}] 成功 ", dispatchType, dispatchNodeDto.getClusterEnName(),
                    targetStorageId, targetRepositoryId, artifactPath);
        } catch (Exception e) {
            if (Boolean.TRUE.equals(recordStatus)) {
                artifactComponent.handlerArtifactPromotion(dispatchNodeDto.getClusterEnName(), srcStorageId, srcRepositoryId, artifactPath, PromotionStatusEnum.FAIL.getStatus());
            }
            log.error("分发 [{} {} {} {} {}] 失败 {} ",
                    dispatchNodeDto.getDispatchType(), dispatchNodeDto.getClusterEnName(),
                    targetStorageId, targetRepositoryId, artifactPath, ExceptionUtils.getStackTrace(e));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    @Async("asyncCopyThreadPoolTaskExecutor")
    public void executeMove(ArtifactPromotion artifactPromotion) {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();
        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        List<TargetRepositoyDto> list = artifactPromotion.getTargetRepositoyList();
        List<FutureTask<String>> listTask = Lists.newArrayList();
        list.forEach(target -> {
            // 多个目标仓库移动
            String targetStorageId = target.getTargetStorageId();
            String targetRepositoryId = target.getTargetRepositoryId();
            Repository targetRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
            FutureTask<String> future = new FutureTask<String>(
                    new ArtifactPromotionCopyTask(srcPath, srcRepository, targetRepository));
            listTask.add(future);
            asyncCopyThreadPoolTaskExecutor.submit(future);
        });
        boolean delFlag = true;
        for (FutureTask<String> task : listTask) {
            try {
                String rs = task.get();
                if (StringUtils.isNotBlank(rs)) {
                    delFlag = false;
                    log.error("Move error [{}]", rs);
                }
            } catch (Exception e) {
                log.error("error [{}]", ExceptionUtils.getStackTrace(e));
            }
        }
        if (delFlag) {
            try {
                artifactManagementService.delete(srcRepositoryPath, true);
            } catch (IOException e) {
                log.error("Delete srcRepositoryPath error [{}]", ExceptionUtils.getStackTrace(e));
            }
        }
        log.info("Execute move params [{}] finished", JSONObject.toJSONString(artifactPromotion));
    }

    public PromotionNodeOptionDto getPromotionUploadDto(PromotionArtifactDto promotionArtifactDto) throws Exception {
        PromotionNodeOptionDto promotionNodeOptionDto = new PromotionNodeOptionDto();
        promotionNodeOptionDto.setStorageId(promotionArtifactDto.getTargetStorageId());
        promotionNodeOptionDto.setRepositoryId(promotionArtifactDto.getTargetRepositoryId());
        Map<String, Map<String, RepositoryPath>> filePathMap = Maps.newLinkedHashMap();
        Map<String, Object> fileMetaDataMap = Maps.newLinkedHashMap();
        handlePromotionUploadPath(promotionArtifactDto, filePathMap, fileMetaDataMap);
        promotionNodeOptionDto.setPathMap(filePathMap);
        promotionNodeOptionDto.setFileMetaDataMap(fileMetaDataMap);
        return promotionNodeOptionDto;
    }

    private void handlePromotionUploadPath(PromotionArtifactDto promotionArtifactDto, Map<String, Map<String, RepositoryPath>> filePathMap, Map<String, Object> fileMetaDataMap) throws Exception {
        String absolutePath = promotionArtifactDto.getPath();
        String tempStr = promotionArtifactDto.getSrcStorageId() + File.separator + promotionArtifactDto.getSrcRepositoryId() + File.separator;
        int fPathIndex = absolutePath.lastIndexOf(tempStr);
        String relativizePath = absolutePath;
        if (fPathIndex != -1) {
            relativizePath = absolutePath.substring(fPathIndex).replace(tempStr, "");
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepositoryId(), relativizePath);
        String layout = repositoryPath.getRepository().getLayout(), srcStorageId = repositoryPath.getStorageId(), srcRepositoryId = repositoryPath.getRepositoryId();
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(layout, repositoryPath);
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(layout);
        for (RepositoryPath srcRepositoryPath : list) {
            if (isDocker) {
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        List<String> layerList = getAllLayerList(manifest);
                        //blobs
                        for (String layer : layerList) {
                            RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                            log.info("Upload find blob srcRepositoryPath [{}]", srcBlobPath);
                            Map<String, RepositoryPath> blobPathMap = Maps.newLinkedHashMap();
                            blobPathMap.put(srcBlobPath.getTarget().toAbsolutePath().toString(), srcBlobPath);
                            filePathMap.put(RepositoryFiles.relativizePath(srcBlobPath), blobPathMap);
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                            log.info("Upload find manifest srcRepositoryPath [{}]", srcMainFestPath);
                            Map<String, RepositoryPath> mainFestPathMap = Maps.newLinkedHashMap();
                            mainFestPathMap.put(srcMainFestPath.getTarget().toAbsolutePath().toString(), srcMainFestPath);
                            filePathMap.put(RepositoryFiles.relativizePath(srcMainFestPath), mainFestPathMap);
                        }
                    }
                }
            }
            log.info("Upload find srcRepositoryPath [{}]", srcRepositoryPath);
            Map<String, RepositoryPath> relativePathMap = Maps.newLinkedHashMap();
            relativePathMap.put(srcRepositoryPath.toAbsolutePath().toString(), srcRepositoryPath);
            filePathMap.put(RepositoryFiles.relativizePath(srcRepositoryPath), relativePathMap);
            // 添加跨节点的元数据同步
            fileMetaDataMap.put(RepositoryFiles.relativizePath(srcRepositoryPath), getMetaData(srcRepositoryPath));
        }
    }

    public void handleCopy(RepositoryPath path, Repository srcRepository, Repository targetRepository) throws Exception {
        final String srcStorageId = srcRepository.getStorage().getId(), srcRepositoryId = srcRepository.getId(),
                targetStorageId = targetRepository.getStorage().getId(), targetRepositoryId = targetRepository.getId();
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(srcRepository.getLayout(), path);
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout());
        for (RepositoryPath srcRepositoryPath : list) {
            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcRepositoryPath));
            if (!RepositoryFiles.isArtifact(srcRepositoryPath)) {
                log.info(String.format("RepositoryPath：%s not is artifact skip", srcRepositoryPath));
                continue;
            }
            if (isDocker) {
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        List<String> layerList = getAllLayerList(manifest);
                        //blobs
                        for (String layer : layerList) {
                            RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                            RepositoryPath targetBlobPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcBlobPath));
                            if (Files.exists(targetBlobPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcBlobPath.toString(), targetBlobPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetBlobPath [{}]", srcBlobPath, targetBlobPath);
                            try (InputStream inputStream = Files.newInputStream(srcBlobPath)) {
                                artifactManagementService.store(targetBlobPath, inputStream);
                            } catch (Exception e) {
                                log.error("Do copy srcRepositoryPath [{}] targetBlobPath [{}] error [{}]", srcBlobPath, targetBlobPath, ExceptionUtils.getStackTrace(e));
                            }
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                            RepositoryPath targetManiFestPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcMainFestPath));
                            if (Files.exists(targetManiFestPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcMainFestPath.toString(), targetManiFestPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcMainFestPath, targetManiFestPath);
                            try (InputStream inputStream = Files.newInputStream(srcMainFestPath)) {
                                artifactManagementService.store(targetManiFestPath, inputStream);
                            } catch (Exception e) {
                                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcMainFestPath, targetManiFestPath, ExceptionUtils.getStackTrace(e));
                            }
                        }
                    }
                }
            }
            log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}]", srcRepositoryPath, targetRepositoryPath);
            try (InputStream is = Files.newInputStream(srcRepositoryPath)) {
                //同步metadata
                setMetaData(targetRepositoryPath, getMetaData(srcRepositoryPath));
                artifactManagementService.store(targetRepositoryPath, is);
            } catch (IOException e) {
                log.error("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] error [{}]", srcRepositoryPath, targetRepositoryPath, ExceptionUtils.getStackTrace(e));
                throw new Exception(e.getMessage());
            }
        }
    }

    public PromotionFileRelativePath getFileRelativePaths(RepositoryPath repositoryPath) throws Exception {
        Map<String, Object> metaData = Maps.newHashMap();
        String layout = repositoryPath.getRepository().getLayout(), srcStorageId = repositoryPath.getStorageId(), srcRepositoryId = repositoryPath.getRepositoryId();
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(layout, repositoryPath);
        List<String> repositoryPaths = Lists.newArrayList();
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(layout);
        for (RepositoryPath srcRepositoryPath : list) {
            if (isDocker) {
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        List<String> layerList = getAllLayerList(manifest);
                        //blobs
                        for (String layer : layerList) {
                            RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                            log.info("Pull find blob srcRepositoryPath [{}]", srcBlobPath);
                            repositoryPaths.add(RepositoryFiles.relativizePath(srcBlobPath));
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                            log.info("Pull find manifest srcRepositoryPath [{}]", srcMainFestPath);
                            repositoryPaths.add(RepositoryFiles.relativizePath(srcMainFestPath));
                        }
                    }
                }
            }
            log.info("Pull find srcRepositoryPath [{}]", srcRepositoryPath);
            repositoryPaths.add(RepositoryFiles.relativizePath(srcRepositoryPath));
            metaData.put(RepositoryFiles.relativizePath(srcRepositoryPath), getMetaData(srcRepositoryPath));
        }
        return new PromotionFileRelativePath(repositoryPaths, metaData);
    }

    /**
     * 以post方式调用第三方接口,以form-data 形式  发送 MultipartFile 文件数据
     *
     * @param url       post请求url
     * @param uploadDto 晋级上传参数实体
     * @return string
     */
    public String upload(String url, PromotionNodeOptionDto uploadDto) throws Exception {
        Response response = null;
        try {
            if (Objects.isNull(uploadDto) || MapUtils.isEmpty(uploadDto.getPathMap())) {
                return "";
            }
            FormDataMultiPart part = new FormDataMultiPart();
            part.field("storageId", uploadDto.getStorageId());
            part.field("repostoryId", uploadDto.getRepositoryId());

            HashMap<String, String> filePathMap = Maps.newLinkedHashMap();
            uploadDto.getPathMap().forEach((x, y) -> {
                y.forEach((j, z) -> {
                    try (InputStream inputStream = Files.newInputStream(z)) {
                        part.bodyPart(new StreamDataBodyPart("files", inputStream, j));
                        filePathMap.put(j, x);
                    } catch (Exception ex) {
                        log.error(ExceptionUtils.getStackTrace(ex));
                        throw new RuntimeException(ex);
                    }
                });
            });
            part.field("filePathMap", JSON.toJSONString(filePathMap));
            part.field("fileMetaDataMap", JSON.toJSONString(uploadDto.getFileMetaDataMap()));
            part.field("promotion", "true");
            Client client = clientPool.getRestClient();
            WebTarget resource = client.register(MultiPartWriter.class).target(url);
            Invocation.Builder builder = resource.request(MediaType.APPLICATION_JSON);
            securityComponent.securityTokenHeader(builder);
            log.info("Upload starting...");
            response = builder.header("Mime-Version", "1.0").
                    post(Entity.entity(part, Boundary.addBoundary(MediaType.MULTIPART_FORM_DATA_TYPE)));
            if (response.getStatus() != 200) {
                log.info("Upload failed [{}]", response.readEntity(String.class));
                throw new Exception("Upload failed");
            }
            String res = response.readEntity(String.class);
            log.info("Upload finished [{}]", res);
        } catch (Exception e) {
            log.error("Upload failed [{}]", ExceptionUtils.getStackTrace(e));
            throw new Exception(e.getMessage());
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
        return "上传成功";
    }

    public String getMetaData(RepositoryPath srcPath) {
        String rs = "";
        try {
            if (Objects.isNull(srcPath) || !Files.exists(srcPath)) {
                return rs;
            }
            Artifact artifact = artifactWebService.getArtifact(srcPath);
            if (Objects.isNull(artifact)) {
                return rs;
            }
            rs = artifact.getMetadata();
        } catch (Exception e) {
            log.error("Exception {}", ExceptionUtils.getStackTrace(e));
        }
        return rs;
    }

    /**
     * 处理metadata
     *
     * @param repositoryPath repositoryPath
     * @param metadata       metadata
     */
    public void setMetaData(RepositoryPath repositoryPath, String metadata) {
        if (Objects.nonNull(repositoryPath) && StringUtils.isNotBlank(metadata) && JSONUtil.isJson(metadata)) {
            try {
                Artifact artifact = Optional.ofNullable(repositoryPath.getArtifactEntry())
                        .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                                RepositoryFiles.readCoordinates(repositoryPath)));
                artifact.setMetadata(metadata);
                repositoryPath.setArtifact(artifact);
            } catch (Exception ex) {
                log.error("setMetaData Exception {} repositoryPath {} metadata {}", ExceptionUtils.getStackTrace(ex), repositoryPath.toString(), metadata);
            }
        }
    }

    public List<String> getAllLayerList(ImageManifest imageManifest) {
        if (Objects.nonNull(imageManifest) && CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
            List<String> layerList = imageManifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
            if (Objects.nonNull(imageManifest.getConfig())) {
                layerList.add(imageManifest.getConfig().getDigest());
            }
            return layerList;
        }
        return Collections.emptyList();
    }

}
