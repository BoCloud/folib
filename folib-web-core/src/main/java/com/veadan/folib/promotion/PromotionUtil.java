package com.veadan.folib.promotion;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.map.MapUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.config.PromotionConfig;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.*;
import com.veadan.folib.dto.*;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.enums.ThreadLocalContextFieldNameEnum;
import com.veadan.folib.forms.common.StorageTreeForm;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import com.veadan.folib.model.request.ArtifactSliceUploadReq;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.RepositoryPathUtil;
import com.veadan.folib.util.ThreadLocalUtil;
import com.veadan.folib.utils.FileUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.wrapper.BufferedInputStreamWrapper;
import com.veadan.folib.ws.client.handler.command.FolibWsClientArtifactPullCommand;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.*;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.InputStreamBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.HttpClients;
import org.glassfish.jersey.media.multipart.Boundary;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.file.StreamDataBodyPart;
import org.glassfish.jersey.media.multipart.internal.MultiPartWriter;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;
import scala.collection.mutable.StringBuilder;

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
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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
    protected ArtifactSyncSlaveRecordMapper artifactSyncSlaveRecordMapper;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Autowired
    private ThreadPoolTaskExecutor asyncCopyThreadPoolTaskExecutor;
    @Autowired
    private ThreadPoolTaskExecutor asyncThreadPoolTaskExecutor;
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
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Autowired
    private PromotionTaskQueue promotionTaskQueue;
    @Autowired
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;
    @Autowired
    private PromotionConfig promotionConfig;
    private static final long MAX_SLICE_BYTE_SIZE = 1024L * 1024L * 100L;//100MB

    @Async("asyncCopyThreadPoolTaskExecutor")
    public void executeCopy(RepositoryPath path, Repository srcRepository, Repository targetRepository) {
        try {
            handleCopy(path, srcRepository, targetRepository);
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path);
        } catch (Exception e) {
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path, ExceptionUtils.getStackTrace(e));
        }
    }

    @Async("asyncCopyThreadPoolTaskExecutor")
    public void executePromotionCopy(String syncNo, String targetPath, RepositoryPath path, Repository srcRepository, Repository targetRepository) {
        String artifactPath = "";
        try {
            artifactPath = RepositoryFiles.relativizePath(path);
            handleCopy(path, srcRepository, targetRepository);
            if (Files.exists(repositoryPathResolver.resolve(targetRepository.getStorage().getId(), targetRepository.getId(), artifactPath))) {
                //插入成功从记录
                insertArtifactSyncSlaveRecord(syncNo, artifactPath, targetPath, ArtifactSyncRecordStatusEnum.SUCCESS.getVal());
            } else {
                insertArtifactSyncSlaveRecord(syncNo, artifactPath, targetPath, ArtifactSyncRecordStatusEnum.FAILED.getVal());
            }
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path);
        } catch (Exception e) {
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path, ExceptionUtils.getStackTrace(e));
            insertArtifactSyncSlaveRecord(syncNo, artifactPath, targetPath, ArtifactSyncRecordStatusEnum.FAILED.getVal());
        }
    }

    //@Async("asyncThreadPoolTaskExecutor")
    public void executeHandleDispatch(ArtifactDispatch artifactDispatch) {
        // 设置上下文字段
        ThreadLocalUtil.set(ThreadLocalContextFieldNameEnum.ARTIFACT_DISPATCH_SYNC_NO.getFieldName(), artifactDispatch.getSyncNo());

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
            handlerDispatch(map, artifactDispatch, targetDispatchRepositoryDto);
        }
    }

    public void handlerDispatch(Map<String, ClusterDispatchNodeDto> map, ArtifactDispatch artifactDispatch,
                                TargetDispatchRepositoryDto targetDispatchRepositoryDto) {
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
            ClusterDispatchNodeDto clusterDispatchNodeDto = dispatchMap.get(dispatchClusterName);
            ArtifactDispatchRepositoryDto dispatchRepositoryDto = ArtifactDispatchRepositoryDto.builder()
                    .type(type)
                    .layout(layout)
                    .dispatchEnName(dispatchClusterName)
                    .policy(policy).build();

            log.info(" 请求分发获取仓库信息 {}", JSONUtil.toJsonStr(dispatchRepositoryDto));

            String targetHostName = FolibWsRunManageUtil.getTargetHostName(clusterDispatchNodeDto);
            WSMessageRequest wsMessageRequest = new WSMessageRequest(Command.STORAGES_REPOSITORY_TREE, dispatchRepositoryDto);
            WSMessageResponse messageResponse = null;
            try {
                messageResponse = folibWsRunManageV2.sendRequest(targetHostName, wsMessageRequest);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            DispatchStorageTree dispatchStorageTree = (DispatchStorageTree) messageResponse.getDate();

            List<StorageTreeForm> storageTreeForms = dispatchStorageTree.getList();
            // 选存储空间下的全部仓库（同类型 同策略 同布局）
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
                        executeDispatchV2(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, tempRepoId, dispatchNodeDto, recordStatus);
                    }
                    break;
                }
            }
        } else {
            executeDispatchV2(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, targetRepositoryId, dispatchNodeDto, recordStatus);
        }
    }

    @Deprecated
    public void executeDispatch(String artifactPath, String srcRepositoryId, String srcStorageId, String targetStorageId, String targetRepositoryId, ClusterDispatchNodeDto dispatchNodeDto, Boolean recordStatus) {
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
            final String syncNo = ThreadLocalUtil.get(ThreadLocalContextFieldNameEnum.ARTIFACT_DISPATCH_SYNC_NO.getFieldName(), String.class);

            log.info("分发 [{}] 开始", dispatchType);
            if (dispatchType.equals("pull")) {
                promotionNodeOption = new PromotionNodeOption(sourcePath, targetPath);
                promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PULL.getVal());
                promotionNodeOption.setSyncNo(syncNo);

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

                // 异步制品切片上传
//                asyncThreadPoolTaskExecutor.submit(() -> {
//                    try {
//                        this.artifactSliceUpload(uploadDto, StringUtils.chomp(dispatchNodeHost, "/"), uploadDto.getStorageId(), uploadDto.getRepostoryId(), syncNo);
//                    } catch (Exception e) {
//                        log.error("异步制品切片上传失败", e);
//                    }
//                });


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

    private void executeDispatchV2(String artifactPath, String srcRepositoryId, String srcStorageId, String targetStorageId, String targetRepositoryId, ClusterDispatchNodeDto dispatchNodeDto, Boolean recordStatus) {
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
            String dispatchType = dispatchNodeDto.getDispatchType();
            final String syncNo = ThreadLocalUtil.get(ThreadLocalContextFieldNameEnum.ARTIFACT_DISPATCH_SYNC_NO.getFieldName(), String.class);

            log.info("分发 [{}] 开始", dispatchType);


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

            final String clusterNodeHost = dispatchNodeDto.getClusterNodeHost();
            final String nodeHost = UrlUtils.getHost(clusterNodeHost);
            final Integer nodePort = UrlUtils.getPort(clusterNodeHost);
            final String nodeName = String.format("%s:%s", nodeHost, nodePort);
            this.artifactSliceUploadV3(uploadDto, StringUtils.chomp(dispatchNodeHost, "/"), uploadDto.getStorageId(), uploadDto.getRepositoryId(), syncNo);
            if (Boolean.TRUE.equals(recordStatus)) {
                artifactComponent.handlerArtifactPromotion(dispatchNodeDto.getClusterEnName(), srcStorageId, srcRepositoryId, artifactPath, PromotionStatusEnum.SUCCESS.getStatus());
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
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            } else {
                throw new RuntimeException(e);
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
                            blobPathMap.put(RepositoryFiles.relativizePath(srcBlobPath), srcBlobPath);
                            filePathMap.put(RepositoryFiles.relativizePath(srcBlobPath), blobPathMap);
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                            log.info("Upload find manifest srcRepositoryPath [{}]", srcMainFestPath);
                            Map<String, RepositoryPath> mainFestPathMap = Maps.newLinkedHashMap();
                            mainFestPathMap.put(RepositoryFiles.relativizePath(srcMainFestPath), srcMainFestPath);
                            filePathMap.put(RepositoryFiles.relativizePath(srcMainFestPath), mainFestPathMap);
                        }
                    }
                }
            }
            log.info("Upload find srcRepositoryPath [{}]", srcRepositoryPath);
            Map<String, RepositoryPath> relativePathMap = Maps.newLinkedHashMap();
            relativePathMap.put(RepositoryFiles.relativizePath(srcRepositoryPath), srcRepositoryPath);
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
                            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcBlobPath, targetBlobPath);
                            try (InputStream inputStream = Files.newInputStream(srcBlobPath)) {
                                artifactManagementService.store(targetBlobPath, inputStream);
                            } catch (Exception e) {
                                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcBlobPath, targetBlobPath, ExceptionUtils.getStackTrace(e));
                                throw new Exception(e.getMessage());
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
                                throw new Exception(e.getMessage());
                            }
                        }
                    }
                }
            }
            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcRepositoryPath, targetRepositoryPath);
            try (InputStream is = Files.newInputStream(srcRepositoryPath)) {
                //同步metadata
                setMetaData(targetRepositoryPath, getMetaData(srcRepositoryPath));
                artifactManagementService.store(targetRepositoryPath, is);
            } catch (IOException e) {
                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcRepositoryPath, targetRepositoryPath, ExceptionUtils.getStackTrace(e));
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

    @Deprecated
    public List<ArtifactSliceUploadHttpEntityResponse> artifactSliceUpload(PromotionNodeOptionDto uploadDto, String targetUrl, String storageId, String repositoryId, String syncNo) {
        targetUrl = StringUtils.chomp(targetUrl, "/");
        final Map<String, Map<String, RepositoryPath>> filePathMap = uploadDto.getPathMap();
        final long sliceByteSize = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
        final HttpClient httpClient = HttpClients.createDefault();
        final HttpPost httpPost = new HttpPost(String.format("%s/api/artifact/folib/promotion/slice/upload", targetUrl));
        final List<PromotionUtil.ArtifactSliceUploadHttpEntityBuilder> artifactSliceUploadHttpEntityList = this.getArtifactSliceUploadHttpEntityList(filePathMap, storageId, repositoryId, sliceByteSize);

        // 记录制品从记录
        String finalTargetUrl = targetUrl;
        artifactSliceUploadHttpEntityList.stream().forEach(e -> {
            String targetPath = String.format("%s/%s/%s/%s-chunk%s?startLength=%s&chunkSize=%s&mergeId=%s", finalTargetUrl, e.getStorageId(), e.getRepositoryId(), e.getPath(), e.getChunkIndex(), e.getStartLength(), e.getChunkSize(), e.getMergeId());
            final ArtifactSyncSlaveRecord artifactSyncSlaveRecord = insertArtifactSyncSlaveRecord(syncNo, e.getPath(), targetPath, ArtifactSyncRecordStatusEnum.IN_SYNC.getVal());
            e.setChunkArtifactRecordId(artifactSyncSlaveRecord.getId());
        });

        return artifactSliceUploadHttpEntityList.stream().map(builder -> {
            httpPost.reset();
            httpPost.setEntity(builder.build());
            final ArtifactSliceUploadHttpEntityResponse res = new ArtifactSliceUploadHttpEntityResponse();
            res.setChunkArtifactRecordId(builder.getChunkArtifactRecordId());
            res.setSuccess(false);
            try {
                final HttpResponse response = httpClient.execute(httpPost);
                int responseCode = response.getStatusLine().getStatusCode();

                res.setSuccess(HttpStatus.OK.value() == responseCode);
                if (!res.getSuccess()) {
                    res.setFailedReason(String.format("上传制品(%s)切片失败", builder.getPath()));
                }
            } catch (IOException e) {
                res.setFailedReason(e.getMessage());
                log.error("制品切片上传失败", e);
            }

            // 更新记录状态
            artifactSyncSlaveRecordMapper.updateRecordStatus(builder.getChunkArtifactRecordId(), res.getSuccess() ? ArtifactSyncRecordStatusEnum.SUCCESS.getVal() : ArtifactSyncRecordStatusEnum.FAILED.getVal(), new Date(), res.getFailedReason());
            return res;
        }).collect(Collectors.toList());
    }

    public CompletableFuture<Void> artifactSliceUploadV3(PromotionNodeOptionDto uploadDto, String targetUrl, String storageId, String repositoryId, String syncNo) {
        targetUrl = StringUtils.chomp(targetUrl, "/");

        String targetHostName = FolibWsRunManageUtil.getTargetHostName(targetUrl);
        TaskQueueManager taskQueueManager = promotionTaskQueue.getTaskQueueManager(targetHostName);
        String finalTargetUrl1 = targetUrl;
        if (taskQueueManager == null) {
            throw new RuntimeException("not found taskQueueManager by targetHostName:"+targetHostName);
        }
        CompletableFuture<Void> future = new CompletableFuture<>();
        taskQueueManager.submitTask(java.util.UUID.randomUUID().toString(), () -> {
            try {
                doArtifactSliceUploadV3(uploadDto, storageId, repositoryId, syncNo, finalTargetUrl1, targetHostName);
                future.complete(null);
            } catch (Exception e) {
                log.error("doArtifactSliceUploadV3 Exception \n info:\nuploadDto:{}, storageId:{}, repositoryId:{}, syncNo:{}, finalTargetUrl1:{}, targetHostName:{}",uploadDto, storageId, repositoryId, syncNo, finalTargetUrl1, targetHostName, e);
                artifactSyncRecordMapper.updateStatusAndFailedReasonBySyncNo(ArtifactSyncRecordStatusEnum.FAILED.getVal(), e.getMessage(), syncNo, new Date());
                future.completeExceptionally(e);
                if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                }
                throw new RuntimeException(e);
            }

        });
        return future;
    }

    private void doArtifactSliceUploadV3(PromotionNodeOptionDto uploadDto, String storageId, String repositoryId, String syncNo, String finalTargetUrl1, String targetHostName) throws Exception {
        final Map<String, Map<String, RepositoryPath>> filePathMap = uploadDto.getPathMap();
        final long sliceByteSize = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
        //从 filePathMap 中移除目标节点中已经存在的制品
        remoteExistsArtifact(storageId, repositoryId, targetHostName, filePathMap, syncNo, finalTargetUrl1);
        if (CollectionUtil.isEmpty(filePathMap)) {
            return;
        }

        final List<ArtifactSliceUploadHttpEntityBuilder> artifactSliceUploadHttpEntityList = this.getArtifactSliceUploadHttpEntityList(filePathMap, storageId, repositoryId, sliceByteSize);

        // 记录制品从记录
        String finalTargetUrl = finalTargetUrl1;
        artifactSliceUploadHttpEntityList.forEach(e -> {
            String targetPath = String.format("%s/%s/%s/%s-chunk%s?startLength=%s&chunkSize=%s&mergeId=%s", finalTargetUrl, e.getStorageId(), e.getRepositoryId(), e.getPath(), e.getChunkIndex(), e.getStartLength(), e.getChunkSize(), e.getMergeId());
            final ArtifactSyncSlaveRecord artifactSyncSlaveRecord = insertArtifactSyncSlaveRecord(syncNo, e.getPath(), targetPath, ArtifactSyncRecordStatusEnum.IN_SYNC.getVal());
            e.setChunkArtifactRecordId(artifactSyncSlaveRecord.getId());
        });
        int size = artifactSliceUploadHttpEntityList.size();
        //分片上传
        for (int i = 0; i < size; i++) {
            ArtifactSliceUploadHttpEntityBuilder builder = artifactSliceUploadHttpEntityList.get(i);
            ArtifactSliceUploadReq artifactSliceUploadReq = builder.buildV3();
            String path = artifactSliceUploadReq.getPath();
            Object metadata = uploadDto.getFileMetaDataMap().get(path);
            artifactSliceUploadReq.setMetaData(JSONObject.parseObject((String) metadata,Map.class));

//            try (InputStream inputStream = artifactSliceUploadReq.getFile().getInputStream();){
//                String md5 = FileUtils.getMD5(inputStream);
//                artifactSliceUploadReq.setSliceMd5(md5);
//            }
            log.info("artifactSliceUploadReq:{}",artifactSliceUploadReq);
            int finalI = i;
            try {
                new RetryTask(promotionConfig.getRetryCount()) {
                    @Override
                    public void exec(RetryTask retryTask) throws Exception {
                        try {
                            log.info("WSMessageRequest upload slice {}/{} ,targetHostName:{} , path:{}", finalI + 1, size, targetHostName, artifactSliceUploadReq.getPath());
                            WSMessageResponse wsMessageResponse = folibWsRunManageV2.sendRequest(targetHostName, new WSMessageRequest(Command.UPLOAD, artifactSliceUploadReq), promotionConfig.getWsRequestTimoutOfArtifactUpload());
                            log.info("wsMessageResponse:{}", wsMessageResponse.toString());
                            if (!HttpStatus.OK.equals(wsMessageResponse.getStatus())) {
                                throw new RuntimeException(String.valueOf(wsMessageResponse.getDate()));
                            }
                        } catch (Exception e) {
                            log.error("upload exception", e);
                            throw e;
                        }
                    }
                }.call();
                artifactSyncSlaveRecordMapper.updateRecordStatus(builder.getChunkArtifactRecordId(), ArtifactSyncRecordStatusEnum.SUCCESS.getVal(), new Date(), "");
            } catch (Exception e) {
                // 更新记录状态
                artifactSyncSlaveRecordMapper.updateRecordStatus(builder.getChunkArtifactRecordId(), ArtifactSyncRecordStatusEnum.FAILED.getVal(), new Date(), e.getMessage());
                throw e;
            }
        }
    }

    @NotNull
    private ArtifactSyncSlaveRecord insertArtifactSyncSlaveRecord(String syncNo, String sourcePath, String targetPath, Integer status) {
        final ArtifactSyncSlaveRecord artifactSyncSlaveRecord = new ArtifactSyncSlaveRecord();
        artifactSyncSlaveRecord.setSourcePath(sourcePath);
        artifactSyncSlaveRecord.setTargetPath(targetPath);
        artifactSyncSlaveRecord.setSyncNo(syncNo);
        artifactSyncSlaveRecord.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
        artifactSyncSlaveRecord.setStatus(status);
//            artifactSyncSlaveRecord.setCreateBy(userName);
        artifactSyncSlaveRecord.setCreateTime(new Date());

        artifactSyncSlaveRecordMapper.insert(artifactSyncSlaveRecord);
        return artifactSyncSlaveRecord;
    }

    private void remoteExistsArtifact(String storageId, String repositoryId, String targetHostName, Map<String, Map<String, RepositoryPath>> filePathMap, String syncNo, String finalTargetUrl1) throws IOException, ExecutionException, InterruptedException, TimeoutException {

        Iterator<Map.Entry<String, Map<String, RepositoryPath>>> iterator = filePathMap.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, Map<String, RepositoryPath>> entry = iterator.next();

            Iterator<Map.Entry<String, RepositoryPath>> interIterator = entry.getValue().entrySet().iterator();
            if (interIterator.hasNext()) {
                Map.Entry<String, RepositoryPath> interEntry = interIterator.next();

                RepositoryPath repositoryPath = interEntry.getValue();


                RepositoryPathExistCheck.RepositoryPathExistCheckBuilder builder = RepositoryPathExistCheck.builder()
                        .storageId(storageId)
                        .repositoryId(repositoryId)
                        .artifactPath(RepositoryFiles.relativizePath(repositoryPath));
                String messageDigestAlgorithms = MessageDigestAlgorithms.SHA_1;
                String layout = repositoryPath.getRepository().getLayout();
                if (DockerLayoutProvider.ALIAS.equals(layout)) {
                    messageDigestAlgorithms = MessageDigestAlgorithms.SHA_256;
                }
                builder.digestAlgorithm(messageDigestAlgorithms);
                LayoutFileSystemProvider provider = repositoryPath.getFileSystem().provider();
                final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, messageDigestAlgorithms);
                if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                    builder.digest(Files.readString(checksumPath));
                } else {
                    log.warn("not found checksumPath ,info: [{}] [{}] [{}] ", checksumPath.getStorageId(), checksumPath.getRepositoryId(), RepositoryFiles.relativizePath(checksumPath));
                    continue;
                }
                RepositoryPathExistCheck repositoryPathExistCheck = builder.build();
                WSMessageRequest wsMessageRequest = new WSMessageRequest(Command.QUERY_ARTIFACT_EXISTS, repositoryPathExistCheck);
                WSMessageResponse wsMessageResponse = null;
                try {
                    wsMessageResponse = folibWsRunManageV2.sendRequest(targetHostName, wsMessageRequest, 60);
                } catch (Exception e) {
                    throw new RuntimeException("QueryArtifactExists exception , repositoryPathExistCheck:" + repositoryPathExistCheck);
                }
                Object date = wsMessageResponse.getDate();
                if (Boolean.TRUE.equals(date)) {
                    iterator.remove();
                    String key = entry.getKey();
                    String targetPath = String.format("%s/%s/%s/%s", finalTargetUrl1, storageId, repositoryId, key);
                    //插入成功从记录
                    insertArtifactSyncSlaveRecord(syncNo, key, targetPath, ArtifactSyncRecordStatusEnum.SUCCESS.getVal());
                }
            } else {
                throw new RuntimeException(String.format("意外的异常，%s中存在超过1个value元素的Map", entry));
            }
        }
    }

    private List<ArtifactSliceUploadHttpEntityBuilder> getArtifactSliceUploadHttpEntityList(Map<String, Map<String, RepositoryPath>> filePathMap, String storageId, String repositoryId, long chunkSize) {
        if (chunkSize <= 0 || chunkSize > MAX_SLICE_BYTE_SIZE) {
            chunkSize = MAX_SLICE_BYTE_SIZE;
            log.info("chunkSize {} exceeds the maximum value {} , use MAX_SLICE_BYTE_SIZE {}", chunkSize, MAX_SLICE_BYTE_SIZE, MAX_SLICE_BYTE_SIZE);
        }
        long finalChunkSize = chunkSize;
        return filePathMap.values().stream().map(m -> {
            return m.entrySet().stream().map(entry -> {
                final String saveUri = entry.getKey();
                final RepositoryPath path = entry.getValue();
                return this.getArtifactSliceUploadHttpEntityList(storageId, repositoryId, saveUri, path, finalChunkSize);
            }).flatMap(Collection::stream).collect(Collectors.toList());
        }).flatMap(Collection::stream).collect(Collectors.toList());
    }


    private List<ArtifactSliceUploadHttpEntityBuilder> getArtifactSliceUploadHttpEntityList(String storageId, String repositoryId, String saveUri, RepositoryPath sourceRepositoryPath, long chunkSize) {
        try {
            final long fileLength = Files.size(sourceRepositoryPath);
            final int threadCount = BigDecimal.valueOf(fileLength).divide(BigDecimal.valueOf(chunkSize), 0, RoundingMode.CEILING).intValue();
            //final String md5 = FileUtils.getMD5(Files.newInputStream(artifactPath));
            final String sourceStorageId = sourceRepositoryPath.getStorageId(), sourceRepositoryId = sourceRepositoryPath.getRepositoryId(), sourceArtifactPath = RepositoryFiles.relativizePath(sourceRepositoryPath);
            log.info("calculate the file [{}] [{}] [{}] md5 file size [{}]", sourceStorageId, sourceRepositoryId, sourceArtifactPath, fileLength);
            long begin = System.currentTimeMillis();
            LayoutFileSystemProvider provider = (LayoutFileSystemProvider) sourceRepositoryPath.getFileSystem().provider();
            final RepositoryPath checksumPath = provider.getChecksumPath(sourceRepositoryPath, MessageDigestAlgorithms.MD5);
            String md5 = "";
            if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                md5 = Files.readString(checksumPath);
            }
            if (StringUtils.isBlank(md5)) {
                md5 = FileUtils.getMD5(Files.newInputStream(sourceRepositoryPath));
            }

            log.info("calculated the file [{}] [{}] [{}] md5 is [{}]  file size [{}] time consuming [{}] ms", sourceStorageId, sourceRepositoryId, sourceArtifactPath, fileLength, md5, System.currentTimeMillis() - begin);
            final String mergeId = UUID.randomUUID().toString(true);

            String finalMd5 = md5;
            return IntStream.range(0, threadCount).mapToObj(index -> {
                long startLength = index * chunkSize;
                try {

                    return new ArtifactSliceUploadHttpEntityBuilder()
                            .setStorageId(storageId)
                            .setRepositoryId(repositoryId)
                            .setPath(saveUri)
                            .setMergeId(mergeId)
                            .setChunkIndex(index + 1)
                            .setChunkIndexMax(threadCount)
                            .setOriginFileMd5(finalMd5)
                            .setArtifactPath(sourceRepositoryPath)
                            .setStartLength(startLength)
                            .setChunkSize(chunkSize);
                } catch (Exception e) {
                    log.error("构建文件切片HttpEntity请求失败", e);
                    return null;
                }
            }).collect(Collectors.toList());
        } catch (Exception e) {
            log.error("构建文件切片请求集合失败", e);
            return Collections.emptyList();
        }
    }


    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityBuilder {
        /**
         * 制品切片记录ID
         */
        private Long chunkArtifactRecordId;
        private String storageId;
        private String repositoryId;
        private String path;
        private String mergeId;
        private Integer chunkIndex;
        private Integer chunkIndexMax;
        private String originFileMd5;
        private Path artifactPath;
        private Long startLength;
        private Long chunkSize;

        public HttpEntity build() {
            try {
                return MultipartEntityBuilder.create()
                        .setContentType(ContentType.MULTIPART_FORM_DATA)
                        .addPart("storageId", new StringBody(storageId))
                        .addPart("repositoryId", new StringBody(repositoryId))
                        .addPart("path", new StringBody(path))
                        .addPart("mergeId", new StringBody(mergeId))
                        .addPart("chunkIndex", new StringBody(String.valueOf(chunkIndex)))
                        .addPart("chunkIndexMax", new StringBody(String.valueOf(chunkIndexMax)))
                        .addPart("originFileMd5", new StringBody(originFileMd5))
                        .addPart("file", new InputStreamBody(new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize), "chunk" + chunkIndex))
                        .build();
            } catch (Exception e) {
                log.error("构建文件切片HttpEntity请求失败", e);
                return null;
            }
        }

        public ArtifactSliceUploadReq buildV3() {
            ArtifactSliceUploadReq artifactSliceUploadReq = new ArtifactSliceUploadReq();
            artifactSliceUploadReq.setStorageId(storageId);
            artifactSliceUploadReq.setRepositoryId(repositoryId);
            artifactSliceUploadReq.setPath(path);
            artifactSliceUploadReq.setMergeId(mergeId);
            artifactSliceUploadReq.setChunkIndex(chunkIndex);
            artifactSliceUploadReq.setChunkIndexMax(chunkIndexMax);
            artifactSliceUploadReq.setOriginFileMd5(originFileMd5);
            // 从文件系统中读取文件
            String name = "file"; // 表单字段名
            String originalFileName = "chunk" + chunkIndex;
            String contentType = "application/octet-stream"; // 文件的内容类型
            byte[] content = new byte[0];
            try (BufferedInputStreamWrapper bufferedInputStreamWrapper = new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize)) {
                content = bufferedInputStreamWrapper.readAllBytes();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            MultipartFile multipartFile = new MockMultipartFile(name, originalFileName, contentType, content);
            artifactSliceUploadReq.setFile(multipartFile);
            return artifactSliceUploadReq;
        }

        public HashMap<String, Object> buildV2() {
            HashMap<String, Object> map = new HashMap<>();
            map.put("storageId", storageId);
            map.put("repositoryId", repositoryId);
            map.put("path", path);
            map.put("mergeId", mergeId);
            map.put("chunkIndex", String.valueOf(chunkIndex));
            map.put("chunkIndexMax", String.valueOf(chunkIndexMax));
            map.put("originFileMd5", originFileMd5);
            BufferedInputStreamWrapper bufferedInputStreamWrapper = null;
            try {
                bufferedInputStreamWrapper = new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            try {
                byte[] bytes = bufferedInputStreamWrapper.readAllBytes();
                map.put("file", bytes);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            return map;
        }
    }

    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityBuilderV2 {
        /**
         * 制品切片记录ID
         */
        private Long chunkArtifactRecordId;
        private String storageId;
        private String repositoryId;
        private String path;
        private String mergeId;
        private Integer chunkIndex;
        private Integer chunkIndexMax;
        private String originFileMd5;
        private Path artifactPath;
        private Long startLength;
        private Long chunkSize;

        public HashMap<String, Object> buildV2() {
            HashMap<String, Object> map = new HashMap<>();
            map.put("storageId", storageId);
            map.put("repositoryId", repositoryId);
            map.put("path", path);
            map.put("mergeId", mergeId);
            map.put("chunkIndex", String.valueOf(chunkIndex));
            map.put("chunkIndexMax", String.valueOf(chunkIndexMax));
            map.put("originFileMd5", originFileMd5);
            try {
                map.put("file", new InputStreamBody(new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize), "chunk" + chunkIndex));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            return map;
        }
    }

    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityResponse {
        /**
         * 制品切片记录ID
         */
        private Long chunkArtifactRecordId;
        private Boolean success;
        private String failedReason;
    }
}
