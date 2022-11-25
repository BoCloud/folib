package com.veadan.folib.services;

import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.dto.PromotionArtifactDto;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * 制品晋级service
 *
 * @author qijianping
 */
public interface ArtifactPromotionService {

    ResponseEntity copy(ArtifactPromotion artifactPromotion);

    ResponseEntity move(ArtifactPromotion artifactPromotion);

    ResponseEntity nodeOption(PromotionNodeOption promotionNodeOption, HttpServletRequest request);

    ResponseEntity upload(MultipartFile[] files,String storageId, String repostoryId, String filePathMap);

    ResponseEntity download(ArtifactDto artifactDto, HttpServletResponse response);

    ResponseEntity getFileRelativePaths(ArtifactDto artifactDto);
}
