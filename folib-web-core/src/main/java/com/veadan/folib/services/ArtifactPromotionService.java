package com.veadan.folib.services;

import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.domain.ArtifactParse;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.model.request.ArtifactPromotionNodeOptionCallbackReq;
import com.veadan.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.veadan.folib.model.request.ArtifactSliceUploadReq;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.veadan.folib.model.response.ArtifactSliceUploadInfoRes;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * 制品晋级service
 *
 * @author qijianping
 */
public interface ArtifactPromotionService {

    ResponseEntity copy(ArtifactPromotion artifactPromotion);

    ResponseEntity move(ArtifactPromotion artifactPromotion);

    @Deprecated
    ResponseEntity<String> nodeOption(PromotionNodeOption promotionNodeOption, HttpServletRequest request);
    CompletableFuture<Void> nodeOptionV2(PromotionNodeOption promotionNodeOption);

    ResponseEntity nodeOptionAttachRecord(PromotionNodeOption promotionNodeOption, String requestHostName, HttpServletResponse response);
    CompletableFuture<Void> uploadArtifact(String syncNo, PromotionNodeOption promotionNodeOption, String requestHostName);
    Boolean artifactPullCallback(ArtifactPromotionNodeOptionCallbackReq model);

    ResponseEntity artifactPromotionInfo(String syncNo);

    ResponseEntity upload(MultipartFile[] files, String storageId, String repositoryId, String filePathMap, String fileMetaDataMap, String uuid, String imageTag, String fileType, String baseUrl, String token);

    ResponseEntity upload(String parseArtifact, String storageId, String repositoryId);

    ResponseEntity download(ArtifactDto artifactDto, HttpServletResponse response);

    ResponseEntity getFileRelativePaths(ArtifactDto artifactDto);

    List<String> artifactDispatchAttachRecord(ArtifactDispatch artifactDispatch, HttpServletRequest request);

    List<String> artifactDispatch(ArtifactDispatch artifactDispatch);

    void validateStorageAndRepository(String storageId, String repositoryId);

    void validateRemoteRepository(String targetNode, String storageId, String repositoryId);

    /**
     * 查询上传进度
     *
     * @param dictType dictType
     * @param uuid     uuid
     * @return 上传进度
     */
    List<Dict> queryUploadProcess(String dictType, String uuid);

    /**
     * 删除上传进度
     *
     * @param dictType dictType
     * @param uuid     uuid
     */
    void deleteUploadProcess(String dictType, String uuid);

    /**
     * 解析制品
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @param file         制品文件
     * @return 制品结果
     */
    ArtifactParse parseArtifact(String storageId, String repositoryId, MultipartFile file);


    /**
     * 限速下载
     * @param repository
     * @param artifactPath
     * @param nodeMark
     * @param response
     * @return
     * @since x.x.x
     */
    Boolean speedLimitDownload(Repository repository, String artifactPath, String nodeMark, HttpServletResponse response);

    /**
     * 切片&限速下载
     * @param repository
     * @param artifactPath
     * @param nodeMark
     * @param artifactMd5
     * @param startDownloadIndex
     * @param readLength
     * @param response
     * @since x.x.x
     */
    Boolean speedLimitSliceDownload(Repository repository, String artifactPath, String nodeMark, String artifactMd5, Long startDownloadIndex, Long readLength, HttpServletResponse response);
    
    
///    /**
///     * 判断制品是否支持切片下载
///     * @param model
///     * @return
///     * @since x.x.x
///     */
///    Boolean querySupportSliceDownload(ArtifactSupportSliceDownloadQueryReq model);
///
///
///    Map<String, Boolean> batchQuerySupportSliceDownload(List<ArtifactSupportSliceDownloadQueryReq> models);
    
///    /**
///     * 获取制品切片下载信息（基于临时文件目录存储）
///     * @param model
///     * @return
///     * @since x.x.x
///     */
///    ArtifactSliceDownloadInfoRes querySliceDownloadInfoStoreTemp(ArtifactSliceDownloadInfoReq model);

    /**
     * 查询制品下载信息
     * @param model
     * @return
     * @since x.x.x
     */
    ArtifactSliceDownloadInfoRes querySliceDownloadInfo(ArtifactSliceDownloadInfoReq model);
    

    List<ArtifactSliceDownloadInfoRes> batchQuerySliceDownloadInfo(List<ArtifactSliceDownloadInfoReq> models);

    /**
     * 查询文件切片预信息
     * @return
     * @since x.x.x
     */
    ArtifactSliceUploadInfoRes querySliceUploadInfo();

    /**
     * 文件切片上传
     *
     * @param model
     * @return
     * @since x.x.x
     */
    Boolean sliceUpload(ArtifactSliceUploadReq model);

}
