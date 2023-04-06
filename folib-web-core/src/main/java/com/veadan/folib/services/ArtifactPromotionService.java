package com.veadan.folib.services;

import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.domain.ArtifactParse;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.entity.Dict;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * 制品晋级service
 *
 * @author qijianping
 */
public interface ArtifactPromotionService {

    ResponseEntity copy(ArtifactPromotion artifactPromotion);

    ResponseEntity move(ArtifactPromotion artifactPromotion);

    ResponseEntity nodeOption(PromotionNodeOption promotionNodeOption, HttpServletRequest request);

    ResponseEntity upload(MultipartFile[] files, String storageId, String repositoryId, String filePathMap, String fileMetaDataMap, String uuid);

    ResponseEntity upload(String parseArtifact, String storageId, String repositoryId);

    ResponseEntity download(ArtifactDto artifactDto, HttpServletResponse response);

    ResponseEntity getFileRelativePaths(ArtifactDto artifactDto);

    ResponseEntity artifactDispatch(ArtifactDispatch artifactDispatch);

    void validateStorageAndRepository(String storageId, String repositoryId) throws Exception;

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

}
