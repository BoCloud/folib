package com.veadan.folib.gremlin.controller;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.gremlin.service.ArtifactWebService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import java.io.IOException;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/artifact")
@Api(value = "/api/artifact")
public class ArtifactController extends BaseController {

    @Inject
    private ArtifactWebService artifactWebService;

    @ApiOperation(value = "导出漏洞的影响范围")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/exportExcel")
    public void exportExcel(@RequestParam(name = "vulnerabilityUuid") String vulnerabilityUuid,
                            @RequestParam(name = "storageId", required = false) String storageId,
                            @RequestParam(name = "repositoryId", required = false) String repositoryId) throws IOException {
        artifactWebService.exportExcel(vulnerabilityUuid, storageId, repositoryId);
    }

}
