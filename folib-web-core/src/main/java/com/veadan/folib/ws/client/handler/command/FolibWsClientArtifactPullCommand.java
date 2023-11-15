package com.veadan.folib.ws.client.handler.command;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.map.MapUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.domain.PromotionFileRelativePath;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes.DownloadPartInfo;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.utils.FileUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.veadan.folib.utils.UrlUtils.parsePath;

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
    private static final String API_ARTIFACT_FOLIB_PROMOTION_GET_FILE_RELATIVE_PATHS = "/api/artifact/folib/promotion/getFileRelativePaths";
    private static final String BATCH_QUERY_ARTIFACT_SUPPORT_SLICE_DOWNLOAD_URL = "/api/artifact/folib/promotion/batch/query/support/slice/download";
    private static final String BATCH_QUERY_ARTIFACT_GET_SLICE_DOWNLOAD_INFO_URL = "/api/artifact/folib/promotion/batch/query/slice/download/info";

    @Autowired
    private ArtifactPromotionService artifactPromotionService;
    @Autowired
    private ArtifactManagementService artifactManagementService;
    @Autowired
    protected ArtifactResolutionService artifactResolutionService;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;


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
        try {
            log.info("进入拉模式={}", true);
            final String sourcePath = promotionNodeOption.getSourcePath();
            final String targetPath = promotionNodeOption.getTargetPath();
            final String srcStorageId = parsePath(sourcePath)[0];
            final String srcRepostoryId = parsePath(sourcePath)[1];
            final String srcUrl = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[0];
            final String srcUri = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[1];
            final String targetStorageId = parsePath(targetPath)[0];
            final String targetRepostoryId = parsePath(targetPath)[1];

            artifactPromotionService.validateStorageAndRepository(targetStorageId, targetRepostoryId);
            // 从源仓路径 pull 到目标仓路径 获取目标主机的path 路径下的文件与目录 然后依次提交到任务队列里面后将文件存入仓库
            final String getFileRelativePathsUrl = srcUrl + API_ARTIFACT_FOLIB_PROMOTION_GET_FILE_RELATIVE_PATHS;
            final ArtifactDto artifactDto = ArtifactDto.builder()
                    .storageId(srcStorageId)
                    .repostoryId(srcRepostoryId)
                    .path(srcUri)
                    .build();
            final Invocation.Builder builder = clientPool.getRestClient()
                    .target(getFileRelativePathsUrl)
                    .request();
            securityComponent.securityTokenHeader(builder);
            final Response response = builder.post(Entity.entity(artifactDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                throw new Exception("{} get error" + getFileRelativePathsUrl);
            }
            // 获取需要拉取的文件集合信息
            final PromotionFileRelativePath promotionFileRelativePath = response.readEntity(PromotionFileRelativePath.class);
            final List<String> getFileRelativePaths = Optional.ofNullable(promotionFileRelativePath.getList()).orElse(Collections.emptyList())
                    .stream()
                    // 排除Slice文件
                    .filter(p -> !p.contains(".slice"))
                    .collect(Collectors.toList());
            final Map<String, Object> metaDataMap = Optional.ofNullable(promotionFileRelativePath.getMetaData()).filter(MapUtil::isNotEmpty).orElse(Collections.emptyMap());

            // 判断是否支持切片下载
            final List<ArtifactSliceDownloadInfoReq> sliceDownloadInfosQueryReq = getFileRelativePaths.stream()
                    .map(path -> new ArtifactSliceDownloadInfoReq()
                            .setStorageId(srcStorageId).setRepositoryId(srcRepostoryId)
                            .setPath(path)
                    ).collect(Collectors.toList());
            final HttpRequest batchQueryArtifactSupportSliceDownloadQueryRequest = HttpUtil.createPost(srcUrl + BATCH_QUERY_ARTIFACT_GET_SLICE_DOWNLOAD_INFO_URL);
            final String bodyJsonStr = JSONUtil.toJsonStr(sliceDownloadInfosQueryReq);
            batchQueryArtifactSupportSliceDownloadQueryRequest.header(JwtTokenFetcher.AUTHORIZATION_HEADER,
                    JwtTokenFetcher.BEARER_AUTHORIZATION_PREFIX + " " + securityComponent.getSecurityToken());
            batchQueryArtifactSupportSliceDownloadQueryRequest.body(bodyJsonStr);
            final HttpResponse sliceDownloadInfosQueryRes = batchQueryArtifactSupportSliceDownloadQueryRequest.execute();
            if (!sliceDownloadInfosQueryRes.isOk()) {
                log.error("批量查询制品切片下载信息失败（{}:{}）", bodyJsonStr, sliceDownloadInfosQueryRes.body());
                throw new BusinessException("批量查询制品切片下载信息失败");
            }
            final String sliceDownloadInfosJson = sliceDownloadInfosQueryRes.body();
            final List<ArtifactSliceDownloadInfoRes> artifactSliceDownloadInfoRes = JSON.parseArray(sliceDownloadInfosJson, ArtifactSliceDownloadInfoRes.class);

            final boolean result = artifactSliceDownloadInfoRes.stream().parallel().allMatch(artifactSliceDownloadInfoRe -> {
//                final String storageId = artifactSliceDownloadInfoRe.getStorageId();
//                final String repositoryId = artifactSliceDownloadInfoRe.getRepositoryId();
                final String path = artifactSliceDownloadInfoRe.getPath();
                final Boolean usedSlice = artifactSliceDownloadInfoRe.getUsedSlice();
                final List<DownloadPartInfo> downloadParInfotList = Optional.ofNullable(artifactSliceDownloadInfoRe.getDownloadPartList()).orElse(Collections.emptyList());
                final RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
                promotionUtil.setMetaData(destPath, String.valueOf(metaDataMap.getOrDefault(path, StringUtils.EMPTY)));

                if (!usedSlice) {
                    // 非切片下载（下载Part有且只有一个）
                    final String artifactFileSliceFolderPath = String.format("%s/artifactTemp/%s", StringUtils.chomp(tempPath, "/"), UUID.fastUUID().toString(true));
                    final String downloadUrl = downloadParInfotList.get(0).getDownloadUrl();
                    try {
                        final String tempPath = String.format("%s/%s", artifactFileSliceFolderPath, FileUtil.getName(path));
                        FileUtil.touch(new File(tempPath));
                        HttpUtil.download(downloadUrl, new FileOutputStream(tempPath), true);
                        final boolean isDocker = destPath.getRepository().getLayout().equalsIgnoreCase("docker");
                        if (isDocker) {
                            if (!path.contains("sha256") && !path.endsWith(".sha256")) {
                                artifactManagementService.store(destPath, Files.newInputStream(Path.of(tempPath)));
                                return true;
                            }
                        }

                        // pull artifact file
                        artifactManagementService.store(destPath, Files.newInputStream(Path.of(tempPath)));

                        // 删除临时目录
                        FileUtil.del(new File(artifactFileSliceFolderPath));
                    } catch (Exception e) {
                        log.error("非切片拉取制品失败（{}）", downloadUrl, e);
                        return false;
                    }
                } else {
                    // 切片下载
                    // - 临时下载到本地
                    final String artifactFileSliceFolderPath = String.format("%s/artifactMerge/%s", StringUtils.chomp(tempPath, "/"), UUID.fastUUID().toString(true));
                    final List<String> sliceFileDownloadPathList = IntStream.range(0, downloadParInfotList.size())
                            .parallel()
                            .mapToObj(index -> {
                                final DownloadPartInfo downloadPartInfo = downloadParInfotList.get(index);
                                final String downloadUrl = downloadPartInfo.getDownloadUrl();
                                final String downloadFilePath = String.format("%s/chunk%s", artifactFileSliceFolderPath, index);
                                final File downloadFile = new File(downloadFilePath);
                                FileUtil.touch(downloadFile);
                                try {
                                    HttpUtil.download(downloadUrl, Files.newOutputStream(downloadFile.toPath()), true);
                                    return new SliceSyncDownloadResult(index, downloadFilePath);
                                } catch (Exception e) {
                                    log.error("切片拉取制品失败（{}）", downloadUrl, e);
                                }
                                return null;
                            })
                            .filter(Objects::nonNull)
                            .sorted(Comparator.comparing(SliceSyncDownloadResult::getOrder))
                            .map(SliceSyncDownloadResult::getDownFilePath)
                            .collect(Collectors.toList());

                    // - 合并
                    if (sliceFileDownloadPathList.size() != downloadParInfotList.size()) {
                        throw new BusinessException("切片文件下载不完整");
                    }
                    final String mergeFilePath = String.format("%s/%s", artifactFileSliceFolderPath, FileUtil.getName(path));
                    FileUtils.mergeFiles(mergeFilePath, sliceFileDownloadPathList);

                    // - 转存到Folib
                    try {
                        artifactManagementService.store(destPath, Files.newInputStream(Path.of(mergeFilePath)));
                    } catch (IOException e) {
                        log.error("转存合并制品文件失败", e);
                        throw new BusinessException("转存合并制品文件失败");
                    }

                    // - 删除本地临时合并目录
                    FileUtil.del(new File(artifactFileSliceFolderPath));
                }
                return true;
            });
            if (!result) {
                log.info("制品拉取失败");
            }

        } catch (Exception e) {
            log.error("拉取制品失败", e);
        }
    }
    
//    private void download(PromotionNodeOption promotionNodeOption) throws Exception {
//        String sourcePath = promotionNodeOption.getSourcePath();
//        String targetPath = promotionNodeOption.getTargetPath();
//        String srcStorageId = parsePath(sourcePath)[0];
//        String srcRepositoryId = parsePath(sourcePath)[1];
//        String srcUrl = sourcePath.split("/" + srcStorageId + "/" + srcRepositoryId + "/")[0];
//        String srcUri = sourcePath.split("/" + srcStorageId + "/" + srcRepositoryId + "/")[1];
//        String targetStorageId = parsePath(targetPath)[0];
//        String targetRepositoryId = parsePath(targetPath)[1];
//
//        artifactPromotionService.validateStorageAndRepository(targetStorageId, targetRepositoryId);
//        // 从源仓路径 pull 到目标仓路径 获取目标主机的path 路径下的文件与目录 然后依次提交到任务队列里面后将文件存入仓库
//        String url = srcUrl + API_ARTIFACT_FOLIB_PROMOTION_GET_FILE_RELATIVE_PATHS;
//        Client client = clientPool.getRestClient();
//        WebTarget target = client.target(url);
//        ArtifactDto artifactDto = ArtifactDto.builder().storageId(srcStorageId).
//                repostoryId(srcRepositoryId).path(srcUri).build();
//        Invocation.Builder builder = target.request();
//        securityComponent.securityTokenHeader(builder);
//        Response response = builder.
//                post(Entity.entity(artifactDto, MediaType.APPLICATION_JSON));
//        if (response.getStatus() != 200) {
//            throw new Exception("{} get error" + url);
//        }
//        PromotionFileRelativePath promotionFileRelativePath = response.readEntity(PromotionFileRelativePath.class);
//        List<String> getFileRelativePaths = promotionFileRelativePath.getList();
//        Map<String, Object> metaDataMap = promotionFileRelativePath.getMetaData();
//        // 添加task
//        List<FutureTask<String>> listTask = new ArrayList<>();
//        for (String path : getFileRelativePaths) {
//            ArtifactDto artifac = ArtifactDto.builder().storageId(srcStorageId)
//                    .repostoryId(srcRepositoryId).path(path).build();
//            String fileUlr = srcUrl + "/api/artifact/folib/promotion/download";
//            String metaData = metaDataMap.getOrDefault(path, "") == null ?
//                    "" : metaDataMap.getOrDefault(path, "").toString();
//            PullArtifactTask pullArtifactTask = new PullArtifactTask(path, fileUlr, targetStorageId,
//                    targetRepositoryId, repositoryPathResolver, artifactManagementService, clientPool,
//                    promotionUtil, artifac, metaData);
//            FutureTask<String> futureTask = new FutureTask<String>(pullArtifactTask);
//            listTask.add(futureTask);
//            asyncRepositoryThreadPoolExecutor.submit(futureTask);
//        }
//        int success = 0;
//        int fail = 0;
//        for (FutureTask<String> task : listTask) {
//            try {
//                task.get();
//                success++;
//
//            } catch (Exception e) {
//                fail++;
//                log.error("pull fail {}", e.getMessage());
//            }
//        }
//        log.info("Handle pulled! Task size {} success {} fail {}", listTask.size(), success, fail);
//        listTask.clear();
//    }
    
    @Data
    @Accessors
    @AllArgsConstructor
    public static class SliceSyncDownloadResult {
        private Integer order;
        private String downFilePath;
    }

}
