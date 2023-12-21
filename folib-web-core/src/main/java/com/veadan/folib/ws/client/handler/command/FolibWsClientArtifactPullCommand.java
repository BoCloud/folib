package com.veadan.folib.ws.client.handler.command;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.map.MapUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import cn.hutool.http.Method;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.domain.PromotionFileRelativePath;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
import com.veadan.folib.model.request.ArtifactPromotionNodeOptionCallbackReq;
import com.veadan.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.veadan.folib.model.request.ArtifactSyncSlaveRecordUpdateReq;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes.DownloadPartInfo;
import com.veadan.folib.model.response.Result;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.utils.FileUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.client.context.FolibWsClientContextInfo;
import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.server.handler.command.FolibWsServerArtifactPullCallbackCommand;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;
import org.springframework.validation.BindingResult;

import javax.ws.rs.core.Response;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.IntStream;

import static com.veadan.folib.utils.UrlUtils.parsePath;
import static java.util.stream.Collectors.toList;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:37
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsClientArtifactPullCommand implements FolibWsClientCommand<PromotionNodeOption> {
    public static final String COMMAND = "/client/artifact/pull";
    /**
     * {@linkplain com.veadan.folib.controllers.promotion.ArtifactPromotionController#getFiles(ArtifactDto, BindingResult)}
     * 查询制品路径下所有制品信息
     */
    private static final String API_ARTIFACT_FOLIB_PROMOTION_GET_FILE_RELATIVE_PATHS = "/api/artifact/folib/promotion/getFileRelativePaths";
    /**
     * {@linkplain com.veadan.folib.controllers.promotion.ArtifactPromotionController#batchQuerySliceDownloadInfo(List)}
     * 查询制品切片下载信息集合
     */
    private static final String BATCH_QUERY_ARTIFACT_GET_SLICE_DOWNLOAD_INFO_URL = "/api/artifact/folib/promotion/batch/query/slice/download/info";
    /**
     * {@linkplain com.veadan.folib.controllers.ArtifactSyncSlaveRecordController#batchAdd(List)}
     * 批量添加从记录
     */
    private static final String BATCH_ADD_ARTIFACT_SYNC_SLAVE_RECORD_POST_URL = "/api/artifactSyncSlaveRecord/batch";
    /**
     * {@linkplain com.veadan.folib.controllers.ArtifactSyncSlaveRecordController#update(ArtifactSyncSlaveRecordUpdateReq)}
     * 更新记录状态
     */
    private static final String UPDATE_ARTIFACT_SYNC_SLAVE_RECORD_PUT_URL = "/api/artifactSyncSlaveRecord";

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

    @Autowired
    private ArtifactManagementService artifactManagementService;

    @Autowired
    protected ArtifactResolutionService artifactResolutionService;

    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    private ThreadPoolTaskExecutor asyncWsCommandThreadPoolTaskExecutor;

    @Autowired
    private PromotionUtil promotionUtil;
    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private SecurityComponent securityComponent;

    @Value("${folib.temp}")
    private String tempPath;


    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(PromotionNodeOption promotionNodeOption) {
        Response response = null;
        try {
            log.info("进入拉模式={}", true);
            final String syncNo = promotionNodeOption.getSyncNo();
            final String sourcePath = UriUtils.decode(StringUtils.removeEnd(promotionNodeOption.getSourcePath(), "/"));
            final String targetPath = UriUtils.decode(StringUtils.removeEnd(promotionNodeOption.getTargetPath(), "/"));
            final String srcStorageId = parsePath(sourcePath)[0];
            final String srcRepostoryId = parsePath(sourcePath)[1];
            final String srcUrl = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[0];
            final String srcUri = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[1];
            final String targetStorageId = parsePath(targetPath)[0];
            final String targetRepostoryId = parsePath(targetPath)[1];

            artifactPromotionService.validateStorageAndRepository(targetStorageId, targetRepostoryId);
            // 从源仓路径 pull 到目标仓路径 获取目标主机的path 路径下的文件与目录 然后依次提交到任务队列里面后将文件存入仓库
            final String getFileRelativePathsUrl = srcUrl + API_ARTIFACT_FOLIB_PROMOTION_GET_FILE_RELATIVE_PATHS;
            // 获取需要拉取的文件集合信息
            final PromotionFileRelativePath promotionFileRelativePath = this.doRequestReturnEntity(Method.POST, getFileRelativePathsUrl, (req) -> {
                final ArtifactDto artifactDto = ArtifactDto.builder()
                        .storageId(srcStorageId)
                        .repostoryId(srcRepostoryId)
                        .path(srcUri)
                        .build();
                req.body(JSON.toJSONString(artifactDto));
            }, PromotionFileRelativePath.class);
            final List<String> getFileRelativePaths = Optional.ofNullable(promotionFileRelativePath.getList()).orElse(Collections.emptyList());
            final Map<String, Object> metaDataMap = Optional.ofNullable(promotionFileRelativePath.getMetaData()).filter(MapUtil::isNotEmpty).orElse(Collections.emptyMap());

            // 判断是否支持切片下载
            final List<ArtifactSliceDownloadInfoReq> sliceDownloadInfosQueryReq = getFileRelativePaths.stream()
                    .map(path -> new ArtifactSliceDownloadInfoReq()
                            .setStorageId(srcStorageId).setRepositoryId(srcRepostoryId)
                            .setPath(path)
                    ).collect(toList());
            List<ArtifactSliceDownloadInfoRes> artifactSliceDownloadInfoRes = this.doRequestReturnListEntity(Method.POST, srcUrl + BATCH_QUERY_ARTIFACT_GET_SLICE_DOWNLOAD_INFO_URL, (req) -> {
                final String bodyJsonStr = JSONUtil.toJsonStr(sliceDownloadInfosQueryReq);
                req.body(bodyJsonStr);
            }, ArtifactSliceDownloadInfoRes.class);
            // 过滤 .sha1、.sha256、.md5文件
            artifactSliceDownloadInfoRes = artifactSliceDownloadInfoRes.stream().filter(e -> {
                final RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, e.getPath());
                try {
                    if (RepositoryFiles.isChecksum(destPath)) {
                        return false;
                    }
                    return true;
                } catch (IOException ex) {
                    return false;
                }
            }).collect(toList());
            // - 获取当前节点标记（用于限速）
            final String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();
            final String nodeMark = String.format("%s:%s", UrlUtils.getHost(baseUrl), UrlUtils.getHost(baseUrl));

            // 生成从记录
            final List<ArtifactSyncSlaveRecord> artifactSyncSlaveRecordList = artifactSliceDownloadInfoRes.stream().map(sliceDownloadInfo -> {
                final List<DownloadPartInfo> downloadPartList = sliceDownloadInfo.getDownloadPartList();
                final String path = sliceDownloadInfo.getPath();

                return downloadPartList.stream().map(p -> {
                    final ArtifactSyncSlaveRecord artifactSyncSlaveRecord = new ArtifactSyncSlaveRecord();
                    artifactSyncSlaveRecord.setSourcePath(path);
                    artifactSyncSlaveRecord.setTargetPath(p.getDownloadUrl());
                    artifactSyncSlaveRecord.setSyncNo(syncNo);
                    artifactSyncSlaveRecord.setSyncModel(ArtifactSyncRecordSyncModelEnum.PULL.getVal());
                    artifactSyncSlaveRecord.setStatus(ArtifactSyncRecordStatusEnum.IN_SYNC.getVal());
///                  artifactSyncSlaveRecord.setCreateBy(userName);
                    artifactSyncSlaveRecord.setCreateTime(new Date());
                    artifactSyncSlaveRecord.setTempId(p.getTemId());
                    return artifactSyncSlaveRecord;
                }).collect(toList());
            }).flatMap(Collection::stream).collect(toList());
            final ResultMapStringLong batchAddSlaveRecordResult = this.doRequestReturnEntity(Method.POST, srcUrl + BATCH_ADD_ARTIFACT_SYNC_SLAVE_RECORD_POST_URL, (req) -> {
                req.body(JSON.toJSONString(artifactSyncSlaveRecordList));
            }, ResultMapStringLong.class);
            if (!batchAddSlaveRecordResult.isSuccess()) {
                throw new BusinessException(batchAddSlaveRecordResult.getMessage());
            }
            final Map<String, Long> temIdToIdMap = batchAddSlaveRecordResult.getData();
            log.info("temIdToIdMap:>>> {}", JSON.toJSONString(temIdToIdMap));

            final boolean result = artifactSliceDownloadInfoRes.stream().allMatch(artifactSliceDownloadInfoRe -> {
                final String path = artifactSliceDownloadInfoRe.getPath();
                final Boolean usedSlice = artifactSliceDownloadInfoRe.getUsedSlice();
                final List<DownloadPartInfo> downloadParInfotList = Optional.ofNullable(artifactSliceDownloadInfoRe.getDownloadPartList()).orElse(Collections.emptyList());
                final RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
///                try {
///                    if (RepositoryFiles.isChecksum(destPath)) {
///                        return true;
///                    }
///                } catch (IOException ex) {
///                    log.error(ExceptionUtils.getStackTrace(ex));
///                }
                final Object metadata = metaDataMap.get(path);
                promotionUtil.setMetaData(destPath, Objects.isNull(metadata) ? StringUtils.EMPTY : metadata.toString());
                final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(destPath.getRepository().getLayout());
                if (!usedSlice) {
                    // 非切片下载（下载Part有且只有一个）
                    final String artifactFileSliceFolderPath = String.format("%s/artifactTemp/%s", StringUtils.chomp(tempPath, "/"), UUID.fastUUID().toString(true));
                    final String downloadUrl = UrlUtils.addQuery(downloadParInfotList.get(0).getDownloadUrl(), "nodeMark", nodeMark);
                    final String temId = downloadParInfotList.get(0).getTemId();
                    int downloadStatus = ArtifactSyncRecordStatusEnum.IN_SYNC.getVal();
                    String failedReason = "";

                    try {
                        final String tempPath = String.format("%s/%s", artifactFileSliceFolderPath, FileUtil.getName(path));
                        FileUtil.touch(new File(tempPath));
                        HttpUtil.download(downloadUrl, new FileOutputStream(tempPath), true);
                        if (isDocker && !path.contains("sha256") && !DockerArtifactCoordinates.exclude(tempPath)) {
                            try (InputStream is = Files.newInputStream(Path.of(tempPath))) {
                                Files.copy(is, destPath);
                            }
                            return true;
                        }

                        // store artifact file
                        artifactManagementService.store(destPath, Files.newInputStream(Path.of(tempPath)));

                        downloadStatus = ArtifactSyncRecordStatusEnum.SUCCESS.getVal();

                        // 删除临时目录
                        FileUtil.del(new File(artifactFileSliceFolderPath));
                    } catch (Exception e) {
                        downloadStatus = ArtifactSyncRecordStatusEnum.FAILED.getVal();
                        failedReason = e.getMessage();
                        log.error("非切片拉取制品失败（{}）", downloadUrl, e);
                        return false;
                    } finally {
                        final ArtifactSyncSlaveRecordUpdateReq artifactSyncSlaveRecordUpdateReq = new ArtifactSyncSlaveRecordUpdateReq()
                                .setId(temIdToIdMap.get(temId))
                                .setStatus(downloadStatus)
                                .setFailedReason(failedReason)
                                .setUpdateTime(new Date());
                        this.updateSlaveRecord(srcUrl, artifactSyncSlaveRecordUpdateReq);
                    }
                } else {
                    // 切片下载
                    final String artifactFileSliceFolderPath = String.format("%s/artifactMerge/%s", StringUtils.chomp(tempPath, "/"), UUID.fastUUID().toString(true));
                    try {
                        // - 临时下载到本地
                        final List<String> sliceFileDownloadPathList = IntStream.range(0, downloadParInfotList.size())
                                .mapToObj(index -> {
                                    final DownloadPartInfo downloadPartInfo = downloadParInfotList.get(index);
                                    final String temId = downloadPartInfo.getTemId();
                                    final String downloadUrl = UrlUtils.addQuery(downloadPartInfo.getDownloadUrl(), "nodeMark", nodeMark);
                                    final String downloadFilePath = String.format("%s/chunk%s", artifactFileSliceFolderPath, index);
                                    final File downloadFile = new File(downloadFilePath);
                                    FileUtil.touch(downloadFile);
                                    int downloadStatus = ArtifactSyncRecordStatusEnum.IN_SYNC.getVal();
                                    String failedReason = "";

                                    try {
                                        HttpUtil.download(downloadUrl, Files.newOutputStream(downloadFile.toPath()), true);
                                        downloadStatus = ArtifactSyncRecordStatusEnum.SUCCESS.getVal();
                                        return new SliceSyncDownloadResult(index, downloadFilePath);
                                    } catch (Exception e) {
                                        downloadStatus = ArtifactSyncRecordStatusEnum.FAILED.getVal();
                                        failedReason = e.getMessage();
                                        log.error("切片拉取制品失败（{}）", downloadUrl, e);
                                    } finally {
                                        final ArtifactSyncSlaveRecordUpdateReq artifactSyncSlaveRecordUpdateReq = new ArtifactSyncSlaveRecordUpdateReq()
                                                .setId(temIdToIdMap.get(temId))
                                                .setStatus(downloadStatus)
                                                .setFailedReason(failedReason)
                                                .setUpdateTime(new Date());
                                        this.updateSlaveRecord(srcUrl, artifactSyncSlaveRecordUpdateReq);
                                    }
                                    return null;
                                })
                                .filter(Objects::nonNull)
                                .sorted(Comparator.comparing(SliceSyncDownloadResult::getOrder))
                                .map(SliceSyncDownloadResult::getDownFilePath)
                                .collect(toList());

                        // - 合并
                        if (sliceFileDownloadPathList.size() != downloadParInfotList.size()) {
                            throw new BusinessException("切片文件下载不完整");
                        }
                        final String mergeFilePath = String.format("%s/%s", artifactFileSliceFolderPath, FileUtil.getName(path));
                        FileUtils.mergeFiles(mergeFilePath, sliceFileDownloadPathList);
                        // - 转存到Folib
                        try {
                            if (isDocker && !path.contains("sha256") && !DockerArtifactCoordinates.exclude(mergeFilePath)) {
                                try (InputStream is = Files.newInputStream(Path.of(mergeFilePath))) {
                                    Files.copy(is, destPath);
                                }
                                return true;
                            }
                            artifactManagementService.store(destPath, Files.newInputStream(Path.of(mergeFilePath)));
                        } catch (IOException e) {
                            log.error("转存合并制品文件失败", e);
                            throw new BusinessException("转存合并制品文件失败");
                        }
                    } finally {
                        // - 删除本地临时合并目录
                        FileUtil.del(new File(artifactFileSliceFolderPath));
                    }
                }
                return true;
            });
            if (!result) {
                throw new BusinessException("制品拉取失败");
            }

            this.resultCallback(ArtifactSyncRecordStatusEnum.SUCCESS.getVal(), null, promotionNodeOption.getSyncNo());
        } catch (Exception e) {
            log.error("拉取制品失败", e);
            this.resultCallback(ArtifactSyncRecordStatusEnum.FAILED.getVal(), e.getMessage(), promotionNodeOption.getSyncNo());
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    private void resultCallback(Integer status, String failedReason, String syncNo) {
        final FolibWsClientContextInfo contextSessionInfo = FolibWsSessionContextHolder.getContextSessionInfo(FolibWsClientContextInfo.class);
        final FolibWsClientRunManage.FolibWsServerRun wsRunInfo = contextSessionInfo.getWsRunInfo();
        wsRunInfo.doAction(new FolibWsAction()
                .command(FolibWsServerArtifactPullCallbackCommand.COMMAND)
                .payload(new ArtifactPromotionNodeOptionCallbackReq()
                        .setStatus(status)
                        .setFailedReason(failedReason)
                        .setSyncNo(syncNo)
                )
        );
    }

    private void updateSlaveRecord(String srcUrl, ArtifactSyncSlaveRecordUpdateReq req) {
        log.info("开始回调（{}:{}）", srcUrl + UPDATE_ARTIFACT_SYNC_SLAVE_RECORD_PUT_URL, JSON.toJSONString(req));
        final Result<Boolean> updateSlaveRecordResult = this.doRequestReturnEntity(Method.PUT, srcUrl + UPDATE_ARTIFACT_SYNC_SLAVE_RECORD_PUT_URL, (request) -> {
            request.body(JSON.toJSONString(req));
        }, Result.class);
        if (!updateSlaveRecordResult.isSuccess()) {
            log.info("回调从记录失败（{}）", updateSlaveRecordResult.getData());
        }
    }

    private <T> T doRequestReturnEntity(Method method, String url, Consumer<HttpRequest> loadReqParamConsumer, Class<T> responseClass) {
        try {
            final HttpRequest request = HttpUtil.createRequest(method, url);
            loadReqParamConsumer.accept(request);
            request.header(JwtTokenFetcher.AUTHORIZATION_HEADER,
                    JwtTokenFetcher.BEARER_AUTHORIZATION_PREFIX + " " + securityComponent.getSecurityToken());
            final HttpResponse response = request.execute();
            final String body = response.body();
            if (!response.isOk()) {
                log.error("发起请求响应状态不正确（RequestURL：{}，ResponseBody：{}）", url, body);
                throw new BusinessException("发起请求响应状态不正确");
            }

            return JSON.parseObject(body, responseClass);
        } catch (Exception e) {
            log.error("发起请求失败（{}）", url, e);
            throw new BusinessException("发起请求失败");
        }
    }

    private <T> List<T> doRequestReturnListEntity(Method method, String url, Consumer<HttpRequest> loadReqParamConsumer, Class<T> responseClass) {
        try {
            final HttpRequest request = HttpUtil.createRequest(method, url);
            loadReqParamConsumer.accept(request);
            request.header(JwtTokenFetcher.AUTHORIZATION_HEADER,
                    JwtTokenFetcher.BEARER_AUTHORIZATION_PREFIX + " " + securityComponent.getSecurityToken());
            final HttpResponse response = request.execute();
            final String body = response.body();
            if (!response.isOk()) {
                log.error("发起请求响应状态不正确（RequestURL：{}，ResponseBody：{}）", url, body);
                throw new BusinessException("发起请求响应状态不正确");
            }

            return JSON.parseArray(body, responseClass);
        } catch (Exception e) {
            log.error("发起请求失败（{}）", url, e);
            throw new BusinessException("发起请求失败");
        }
    }

    @Data
    @Accessors
    @AllArgsConstructor
    public static class SliceSyncDownloadResult {
        private Integer order;
        private String downFilePath;
    }

    
    @Data
    @ToString(callSuper = true)
    @EqualsAndHashCode(callSuper = false)
    public static class ResultMapStringLong extends Result<Map<String, Long>> {
        
    }
}
