package com.veadan.folib.services;

import com.veadan.folib.domain.ArtifactPromotion;
import org.springframework.http.ResponseEntity;

/**
 * 制品晋级service
 *
 * @author qijianping
 */
public interface ArtifactPromotionService {

    ResponseEntity copy(ArtifactPromotion artifactPromotion);

    ResponseEntity move(ArtifactPromotion artifactPromotion);

}
