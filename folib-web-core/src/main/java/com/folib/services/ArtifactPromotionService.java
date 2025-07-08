package com.folib.services;

import com.folib.domain.ArtifactParse;
import com.folib.domain.ArtifactPromotion;
import com.folib.dto.ArtifactDto;
import com.folib.entity.Dict;
import com.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.folib.model.request.ArtifactSliceUploadReq;
import com.folib.model.request.ArtifactSliceUploadWebReq;
import com.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.folib.model.response.ArtifactSliceUploadInfoRes;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * 制品晋级service
 *
 * @author veadan
 */
public interface ArtifactPromotionService {

    ResponseEntity syncCopy(ArtifactPromotion artifactPromotion);

    ResponseEntity syncMove(ArtifactPromotion artifactPromotion);

    ResponseEntity copy(ArtifactPromotion artifactPromotion);

    ResponseEntity move(ArtifactPromotion artifactPromotion);

    ResponseEntity upload(MultipartFile[] files, String storageId, String repositoryId, String filePathMap, String fileMetaDataMap, String uuid, String imageTag, String fileType, String baseUrl, String token);

    ResponseEntity upload(String parseArtifact, String storageId, String repositoryId);

    ResponseEntity download(ArtifactDto artifactDto, HttpServletResponse response);

    ResponseEntity getFileRelativePaths(ArtifactDto artifactDto);

    void validateStorageAndRepository(String storageId, String repositoryId);


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
     * 查询制品下载信息
     * @param model
     * @return
     */
    ArtifactSliceDownloadInfoRes querySliceDownloadInfo(ArtifactSliceDownloadInfoReq model);
    

    List<ArtifactSliceDownloadInfoRes> batchQuerySliceDownloadInfo(List<ArtifactSliceDownloadInfoReq> models);

    /**
     * 查询文件切片预信息
     * @return
     */
    ArtifactSliceUploadInfoRes querySliceUploadInfo();

    /**
     * 文件切片上传
     *
     * @param model
     * @return
     */
    Boolean sliceUpload(ArtifactSliceUploadReq model);

    /**
     * 文件切片上传
     *
     * @param model
     * @return
     */
    Boolean sliceUpload(ArtifactSliceUploadReq model, String metaDataMap);


   /**
    * web切片上传
    * @param model
    * @return
    */
   Boolean webSliceUpload(ArtifactSliceUploadWebReq model);

}
