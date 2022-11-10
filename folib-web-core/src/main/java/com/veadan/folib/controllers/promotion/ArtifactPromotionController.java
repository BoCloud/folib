package com.veadan.folib.controllers.promotion;

import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 制品晋级控制层
 *
 * @author qijianping
 */
@RestController
@RequestMapping("/api/artifact/folib/promotion")
@Api(value = "/api/artifact/folib/promotion")
@Slf4j
public class ArtifactPromotionController extends BaseArtifactController {

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

    @PostMapping("/copy")
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    public ResponseEntity copy(@RequestBody @Validated ArtifactPromotion artifactPromotion,
                               BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.copy(artifactPromotion);
    }

    @PostMapping("/move")
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    public ResponseEntity move(@RequestBody @Validated ArtifactPromotion artifactPromotion, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.move(artifactPromotion);
    }

}
