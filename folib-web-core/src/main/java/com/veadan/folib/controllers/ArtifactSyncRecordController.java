package com.veadan.folib.controllers;

import com.veadan.folib.model.request.ArtifactSyncRecordPageReq;
import com.veadan.folib.model.response.ArtifactSyncRecordPageRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ArtifactSyncRecordService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 14:36
 * @since x.x.x
 */
@RestController
@RequestMapping("/api/artifactSyncRecord")
@Api(value = "制品晋级/分发记录", description = "制品管理", tags = "制品管理")
public class ArtifactSyncRecordController {
    
    @Autowired
    private ArtifactSyncRecordService artifactSyncRecordService;
    
    @ApiOperation(value = "制品晋级/分发记录分页查询")
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<ArtifactSyncRecordPageRes> page(ArtifactSyncRecordPageReq model) throws IOException {
        return artifactSyncRecordService.page(model);
    }
}
