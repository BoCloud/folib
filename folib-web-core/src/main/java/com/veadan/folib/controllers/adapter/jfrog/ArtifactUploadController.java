package com.veadan.folib.controllers.adapter.jfrog;

import com.veadan.folib.controllers.BaseController;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author leipenghui
 */
@Slf4j
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog上传", tags = "JFrog上传")
public class ArtifactUploadController extends BaseController {

}
